"""Prepares a friction audit: picks this machine's Claude Code sessions in a time
window, turns each into readable text parts, counts mechanical friction signals,
and packs sessions into worker-sized batches so /friction-audit can fan out.

Usage:
  python -I -X utf8 friction_audit.py prepare <since> <out dir> [--exclude <session id>]

<since>: Nd (last N days) | YYYY-MM-DD | auto (after the newest report in
~/friction-audit/, or the last 30 days when there is none).

Writes into <out dir>: sessions/<id8>/part-<n>.txt, signals.jsonl, batches.json,
summary.json. Prints the summary.
"""

from __future__ import annotations

import importlib.util
import json
import re
import sys
from collections import defaultdict
from datetime import datetime, timedelta
from pathlib import Path

REPORT_DIR = Path.home() / "friction-audit"
REPORT_NAME_RE = re.compile(r"^(\d{4}-\d{2}-\d{2}-\d{4})\.org$")
DAYS_RE = re.compile(r"^(\d+)d$")
DATE_RE = re.compile(r"^\d{4}-\d{2}-\d{2}$")
DEFAULT_DAYS = 30
MIN_UTTERANCES = 3
BATCH_BYTES = 250_000
SHORT_UTTERANCE_CHARS = 30
LONG_PASTE_LINES = 8
EXAMPLES_PER_SIGNAL = 5

PATH_LINE_RE = re.compile(r"[\w./\\:-]+\.\w+:\d+")
CORRECTION_RE = re.compile(r"이상|깨졌|깨짐|왜 이렇게|열어보니|잘못|틀렸|아니라|다시 해|되돌|번복|안 돼|안됨|안 됨|원래대로")
INTERRUPT_RE = re.compile(r"Request interrupted|interrupted by user", re.I)
DENIED_RE = re.compile(r"user doesn't want to proceed|was rejected|permission denied|has been denied|denied by", re.I)


def load_extractor():
    """session-notes owns transcript parsing; reuse it instead of a second parser."""
    path = Path(__file__).resolve().parent.parent / "session-notes" / "extract_session.py"
    if not path.is_file():
        sys.exit(f"missing dependency: {path}")
    spec = importlib.util.spec_from_file_location("extract_session", path)
    module = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(module)
    return module


ex = load_extractor()


def latest_report() -> Path | None:
    if not REPORT_DIR.is_dir():
        return None
    reports = sorted(p for p in REPORT_DIR.iterdir() if REPORT_NAME_RE.match(p.name))
    return reports[-1] if reports else None


def report_time(path: Path) -> datetime:
    return datetime.strptime(REPORT_NAME_RE.match(path.name).group(1), "%Y-%m-%d-%H%M")


def parse_since(arg: str, now: datetime) -> tuple[datetime, str]:
    days = DAYS_RE.match(arg)
    if days:
        return now - timedelta(days=int(days.group(1))), f"최근 {days.group(1)}일"
    if DATE_RE.match(arg):
        return datetime.strptime(arg, "%Y-%m-%d"), f"{arg} 이후"
    if arg == "auto":
        last = latest_report()
        if last:
            return report_time(last), f"지난 보고서 {last.name} 이후"
        return now - timedelta(days=DEFAULT_DAYS), f"보고서 없음 - 최근 {DEFAULT_DAYS}일"
    sys.exit(f"invalid <since>: {arg!r} (use Nd, YYYY-MM-DD or auto)")


def is_human(rec: dict) -> bool:
    return rec.get("origin", {}).get("kind") == "human" or rec.get("turnOrigin") == "human"


def human_utterances(records: list[dict]) -> list[tuple[str, str, str]]:
    """(timestamp, raw text, cleaned text) per human message."""
    out = []
    for rec in records:
        if rec.get("type") != "user" or rec.get("isSidechain") or rec.get("isMeta") or rec.get("isCompactSummary"):
            continue
        if not is_human(rec):
            continue
        raw = "\n".join(b.get("text", "") for b in ex.blocks(rec) if b.get("type") == "text")
        cleaned = ex.clean_user_text(raw)
        if cleaned:
            out.append((rec.get("timestamp", ""), raw, cleaned))
    return out


def count_signals(session: str, project: str, records: list[dict], utterances: list) -> dict:
    corrections = [(t, c) for t, _, c in utterances if CORRECTION_RE.search(c)]
    tool_calls: dict[str, int] = defaultdict(int)
    names: dict[str, str] = {}
    errors = denied = interrupts = 0
    results: list[tuple[int, str]] = []
    for rec in records:
        if rec.get("isSidechain"):
            continue
        for b in ex.blocks(rec):
            kind = b.get("type")
            if kind == "tool_use":
                tool_calls[b.get("name", "?")] += 1
                names[b.get("id", "")] = b.get("name", "?")
            elif kind == "tool_result":
                text = ex.result_text(b)
                if b.get("is_error"):
                    errors += 1
                    denied += bool(DENIED_RE.search(text))
                results.append((len(text), names.get(b.get("tool_use_id", ""), "?")))
            elif kind == "text" and INTERRUPT_RE.search(b.get("text", "")):
                interrupts += 1
    return {
        "session": session,
        "project": project,
        "utterances": len(utterances),
        "E1_path_line": sum(len(PATH_LINE_RE.findall(c)) for _, _, c in utterances),
        "E1_attach": sum(r.count("[@") for _, r, _ in utterances),
        "E1_long_paste": sum(1 for _, _, c in utterances if c.count("\n") >= LONG_PASTE_LINES),
        "E2_correction": len(corrections),
        "E2_examples": [f"{ex.local_time(t) if t else '?'} {c[:80]!r}" for t, c in corrections[:EXAMPLES_PER_SIGNAL]],
        "E4_tool_errors": errors,
        "E4_denied": denied,
        "E4_interrupts": interrupts,
        "tool_calls": dict(sorted(tool_calls.items(), key=lambda kv: -kv[1])),
        "big_results": [f"{n}B {name}" for n, name in sorted(results, reverse=True)[:3]],
    }


def write_parts(records: list[dict], out_dir: Path) -> int:
    out_dir.mkdir(parents=True, exist_ok=True)
    total = 0
    for i, part in enumerate(ex.split_parts(ex.render(records)), 1):
        path = out_dir / f"part-{i}.txt"
        with path.open("w", encoding="utf-8", newline="\n") as f:
            f.write(part)
        total += path.stat().st_size
    return total


def pack(sizes: dict[str, int]) -> list[list[str]]:
    """First-fit decreasing; a session bigger than one batch gets its own."""
    batches: list[tuple[int, list[str]]] = []
    for session, size in sorted(sizes.items(), key=lambda kv: -kv[1]):
        for i, (used, members) in enumerate(batches):
            if used + size <= BATCH_BYTES:
                batches[i] = (used + size, members + [session])
                break
        else:
            batches.append((size, [session]))
    return [members for _, members in batches]


def write_json(path: Path, data) -> None:
    with path.open("w", encoding="utf-8", newline="\n") as f:
        json.dump(data, f, ensure_ascii=False, indent=1)


def prepare(since_arg: str, out_dir: Path, exclude: str | None) -> None:
    now = datetime.now()
    since, since_label = parse_since(since_arg, now)
    cutoff = since.timestamp()
    out_dir.mkdir(parents=True, exist_ok=True)

    signals, sizes, sessions, short = [], {}, [], defaultdict(set)
    bad_lines = 0
    for jsonl in sorted(ex.PROJECTS_DIR.glob("*/*.jsonl")):
        if jsonl.stat().st_mtime < cutoff or (exclude and jsonl.stem.startswith(exclude)):
            continue
        records, bad = ex.load_records(jsonl)
        bad_lines += bad
        utterances = human_utterances(records)
        if len(utterances) < MIN_UTTERANCES:
            continue
        id8 = jsonl.stem[:8]
        times = [r["timestamp"] for r in records if r.get("timestamp") and r.get("type") in ("user", "assistant")]
        sizes[id8] = write_parts(records, out_dir / "sessions" / id8)
        signals.append(count_signals(id8, jsonl.parent.name, records, utterances))
        sessions.append({
            "session": id8,
            "project": jsonl.parent.name,
            "jsonl": jsonl.as_posix(),
            "start": ex.local_time(times[0]) if times else "?",
            "end": ex.local_time(times[-1]) if times else "?",
            "utterances": len(utterances),
            "bytes": sizes[id8],
        })
        for _, _, text in utterances:
            if len(text.strip()) <= SHORT_UTTERANCE_CHARS:
                short[re.sub(r"\s+", " ", text.strip().lower())].add(id8)

    previous = latest_report()
    batches = pack(sizes)
    by_project: dict[str, int] = defaultdict(int)
    for s in sessions:
        by_project[s["project"]] += 1
    repeated = sorted(((len(v), k) for k, v in short.items() if len(v) >= 2), reverse=True)
    summary = {
        "since": since.strftime("%Y-%m-%d %H:%M"),
        "until": now.strftime("%Y-%m-%d %H:%M"),
        "since_label": since_label,
        "sessions": len(sessions),
        "batches": len(batches),
        "by_project": dict(sorted(by_project.items(), key=lambda kv: -kv[1])),
        "previous_report": previous.as_posix() if previous else None,
        "skipped_bad_lines": bad_lines,
        "repeated_short_utterances": repeated[:30],
        "out_dir": out_dir.as_posix(),
    }

    with (out_dir / "signals.jsonl").open("w", encoding="utf-8", newline="\n") as f:
        for s in signals:
            f.write(json.dumps(s, ensure_ascii=False) + "\n")
    write_json(out_dir / "batches.json", batches)
    write_json(out_dir / "sessions.json", sessions)
    write_json(out_dir / "summary.json", summary)
    print(json.dumps(summary, ensure_ascii=False, indent=1))
    if not sessions:
        print("no sessions in range")


def main() -> None:
    sys.stdout.reconfigure(encoding="utf-8")
    args = sys.argv[1:]
    exclude = None
    if "--exclude" in args:
        i = args.index("--exclude")
        if i + 1 >= len(args):
            sys.exit("--exclude needs a session id")
        exclude = args[i + 1].strip()
        if exclude.startswith("${"):  # placeholder left unsubstituted
            exclude = None
        del args[i:i + 2]
    if len(args) != 3 or args[0] != "prepare":
        sys.exit(__doc__)
    prepare(args[1].strip(), Path(args[2]), exclude)


if __name__ == "__main__":
    main()

"""Turns the current Claude Code session transcript (jsonl) into plain text so
session-notes workers can read the whole conversation, including what compaction
dropped from the live context.

Usage:
  python -I extract_session.py <session id | path.jsonl> <out dir>
  python -I extract_session.py <session id | path.jsonl> --docs

Text mode writes part-<n>.txt files (split at user-utterance boundaries) plus
meta.txt into <out dir> and prints their paths. --docs prints the .org files
the session edited or mentioned by absolute path, one per line.
"""

from __future__ import annotations

import json
import os
import re
import sys
from datetime import datetime
from pathlib import Path

PROJECTS_DIR = Path.home() / ".claude" / "projects"
SESSION_ID_RE = re.compile(r"^[0-9a-f]{8}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{12}$")
BASH_HEAD_LINES = 15
BASH_TAIL_LINES = 15
MAX_PART_CHARS = 120_000
ATTACHED_CONTEXT_RE = re.compile(r"<context ref=\"([^\"]*)\">.*?</context>", re.S)
SYSTEM_REMINDER_RE = re.compile(r"<system-reminder>.*?</system-reminder>", re.S)
COMMAND_NAME_RE = re.compile(r"<command-name>(.*?)</command-name>", re.S)
COMMAND_ARGS_RE = re.compile(r"<command-args>(.*?)</command-args>", re.S)
LOCAL_COMMAND_RE = re.compile(r"<local-command-(?:stdout|stderr|caveat)>.*?</local-command-(?:stdout|stderr|caveat)>", re.S)
WRITE_TOOLS = ("Write", "Edit", "NotebookEdit")
# Absolute paths only: drive (C:/), home (~/), or root (/...; also MSYS /c/...).
# Lookbehinds keep URL pieces like "https://h/x.org" from matching.
ORG_PATH_RE = re.compile(r"(?:(?<![A-Za-z])[A-Za-z]:[/\\]|~/|(?<![\w.:/])/)[^\s'\"`<>|:*?]*?\.org(?![\w.])")
MSYS_DRIVE_RE = re.compile(r"^/([a-zA-Z])/")


def find_transcript(arg: str) -> Path:
    """Falls back to the newest transcript only when the skill's session id
    placeholder was left unsubstituted; any other unusable argument is an error
    so a typo never silently reads the wrong session."""
    arg = arg.strip()
    if arg.endswith(".jsonl"):
        path = Path(arg)
        if not path.is_file():
            sys.exit(f"transcript not found: {path}")
        return path
    if SESSION_ID_RE.match(arg):
        hits = list(PROJECTS_DIR.glob(f"*/{arg}.jsonl"))
        if not hits:
            sys.exit(f"no transcript for session {arg} under {PROJECTS_DIR}")
        return hits[0]
    if arg and not arg.startswith("${"):
        sys.exit(f"not a session id or .jsonl path: {arg!r}")
    newest = max(PROJECTS_DIR.glob("*/*.jsonl"), key=lambda p: p.stat().st_mtime, default=None)
    if newest is None:
        sys.exit(f"no transcripts under {PROJECTS_DIR}")
    print(f"warning: session id placeholder unsubstituted, using newest: {newest}", file=sys.stderr)
    return newest


def load_records(path: Path) -> tuple[list[dict], int]:
    records, bad = [], 0
    with path.open(encoding="utf-8") as f:
        for line in f:
            try:
                records.append(json.loads(line))
            except json.JSONDecodeError:
                bad += 1
    return records, bad


def local_time(ts: str) -> str:
    return datetime.fromisoformat(ts.replace("Z", "+00:00")).astimezone().strftime("%Y-%m-%d %H:%M")


def blocks(record: dict) -> list[dict]:
    content = record.get("message", {}).get("content")
    if isinstance(content, str):
        return [{"type": "text", "text": content}]
    return content or []


def clean_user_text(text: str) -> str:
    text = ATTACHED_CONTEXT_RE.sub(lambda m: f"[첨부 파일 내용 생략: {m.group(1)}]", text)
    text = SYSTEM_REMINDER_RE.sub("", text)
    text = LOCAL_COMMAND_RE.sub("", text)
    name = COMMAND_NAME_RE.search(text)
    if name:
        args = COMMAND_ARGS_RE.search(text)
        return f"{name.group(1)} {args.group(1).strip() if args else ''}".strip()
    return text.strip()


def result_text(block: dict) -> str:
    content = block.get("content")
    if isinstance(content, list):
        content = "\n".join(b.get("text", "") for b in content if b.get("type") == "text")
    return content or ""


def clip(text: str) -> str:
    lines = text.splitlines()
    if len(lines) <= BASH_HEAD_LINES + BASH_TAIL_LINES:
        return text
    skipped = len(lines) - BASH_HEAD_LINES - BASH_TAIL_LINES
    return "\n".join(lines[:BASH_HEAD_LINES] + [f"... ({skipped}줄 생략)"] + lines[-BASH_TAIL_LINES:])


def tool_summary(block: dict) -> str:
    name, inp = block.get("name", "?"), block.get("input", {})
    if name == "Bash":
        return f"Bash: {inp.get('description', '')}\n$ {inp.get('command', '')}"
    if name in WRITE_TOOLS or name == "Read":
        return f"{name}: {inp.get('file_path') or inp.get('notebook_path', '')}"
    if name in ("Agent", "Task"):
        return f"{name}: {inp.get('description', '')}"
    if name == "Skill":
        return f"Skill: {inp.get('skill', '')} {inp.get('args', '')}".strip()
    return name


def render(records: list[dict]) -> list[str]:
    """Returns entries; each user utterance starts a new entry tagged U<n>."""
    tool_names: dict[str, str] = {}
    entries: list[str] = []
    utterance = 0
    for rec in records:
        kind = rec.get("type")
        if kind not in ("user", "assistant") or rec.get("isSidechain"):
            continue
        if rec.get("isMeta") or rec.get("isCompactSummary"):
            continue
        when = local_time(rec["timestamp"]) if rec.get("timestamp") else "?"
        if kind == "user":
            # One message may arrive as several text blocks (prompt, @file, selection).
            parts = (clean_user_text(b.get("text", "")) for b in blocks(rec) if b.get("type") == "text")
            text = "\n\n".join(p for p in parts if p)
            if text and (rec.get("origin", {}).get("kind") == "human" or rec.get("turnOrigin") == "human"):
                utterance += 1
                entries.append(f"\n=== U{utterance} [{when}] 사용자\n{text}")
            elif text:
                entries.append(f"[{when}] 시스템\n{text}")
        for block in blocks(rec):
            btype = block.get("type")
            if kind == "user" and btype == "tool_result":
                name = tool_names.get(block.get("tool_use_id", ""), "")
                text = result_text(block)
                if name == "Bash" or block.get("is_error"):
                    entries.append(f"[{when}] 도구 결과({name or '?'}{', 오류' if block.get('is_error') else ''})\n{clip(text)}")
            elif kind == "assistant" and btype == "text":
                entries.append(f"[{when}] 에이전트\n{block.get('text', '').strip()}")
            elif kind == "assistant" and btype == "tool_use":
                tool_names[block.get("id", "")] = block.get("name", "")
                entries.append(f"[{when}] 에이전트 도구 {tool_summary(block)}")
    return entries


def split_parts(entries: list[str]) -> list[str]:
    parts, current = [], ""
    for entry in entries:
        starts_utterance = entry.startswith("\n=== U")
        if starts_utterance and current and len(current) + len(entry) > MAX_PART_CHARS:
            parts.append(current)
            current = ""
        current += entry + "\n"
    if current:
        parts.append(current)
    return parts


def normalize_path(raw: str) -> str:
    msys = MSYS_DRIVE_RE.match(raw) if os.name == "nt" else None  # /c/... is a drive only on Windows
    if msys:
        raw = f"{msys.group(1).upper()}:/{raw[3:]}"
    return Path(raw).expanduser().as_posix()


def mentioned_org_files(records: list[dict]) -> list[str]:
    """Org files the session edited or talked about: Write/Edit targets first,
    then absolute .org paths in Bash commands and user messages (edits made via
    scripts only show up there)."""
    seen: dict[str, str] = {}  # normcase key -> first spelling
    texts: list[str] = []
    for rec in records:
        if rec.get("isSidechain") or rec.get("isMeta") or rec.get("isCompactSummary"):
            continue
        for block in blocks(rec):
            if block.get("type") == "tool_use":
                inp = block.get("input", {})
                if block.get("name") in WRITE_TOOLS and inp.get("file_path", "").lower().endswith(".org"):
                    path = normalize_path(inp["file_path"])
                    seen.setdefault(os.path.normcase(path), path)
                elif block.get("name") == "Bash":
                    texts.append(inp.get("command", ""))
            elif rec.get("type") == "user" and block.get("type") == "text":
                texts.append(ATTACHED_CONTEXT_RE.sub("", block.get("text", "")))
    for text in texts:
        for m in ORG_PATH_RE.finditer(text):
            path = normalize_path(m.group(0))
            seen.setdefault(os.path.normcase(path), path)
    return [p for p in seen.values() if Path(p).is_file()]


def main() -> None:
    sys.stdout.reconfigure(encoding="utf-8")
    if len(sys.argv) != 3:
        sys.exit(__doc__)
    transcript = find_transcript(sys.argv[1])
    records, bad = load_records(transcript)
    if sys.argv[2] == "--docs":
        print("\n".join(mentioned_org_files(records)))
        return

    out_dir = Path(sys.argv[2])
    out_dir.mkdir(parents=True, exist_ok=True)
    times = [r["timestamp"] for r in records if r.get("timestamp") and r.get("type") in ("user", "assistant")]
    parts = split_parts(render(records))
    meta = [
        f"transcript: {transcript}",
        f"session: {transcript.stem}",
        f"range: {local_time(times[0]) if times else '?'} ~ {local_time(times[-1]) if times else '?'}",
        f"skipped bad lines: {bad}",
        f"parts: {len(parts)}",
    ]
    (out_dir / "meta.txt").write_text("\n".join(meta) + "\n", encoding="utf-8")
    print("\n".join(meta))
    for i, part in enumerate(parts, 1):
        path = out_dir / f"part-{i}.txt"
        path.write_text(part, encoding="utf-8")
        print(path)


if __name__ == "__main__":
    main()

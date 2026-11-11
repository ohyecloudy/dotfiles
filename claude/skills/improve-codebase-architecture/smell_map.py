"""
Measure where a repository is rotting so the architecture review starts from evidence, not guesswork.

Reads git history once (`--name-only`, no content diffs) and emits the "smell map" for improve-codebase-architecture:
hotspots (change frequency x size), cross-boundary change coupling, and net growth.
Each section comes with ready-to-embed mermaid source, so the notation pitfalls
(quoted quadrant labels, bar colors) are fixed here instead of in the agent.

Usage:
    python smell_map.py [--repo DIR] [--since-days N] [--path P] [--exclude GLOB ...]

Output: JSON object to stdout (UTF-8). Progress info goes to stderr.
Sizes come from the working tree (so git-crypt files are measured decrypted);
files whose content has a NUL byte are treated as binary and skipped.
"""

import argparse
import datetime
import fnmatch
import itertools
import json
import math
import os
import posixpath
import subprocess
import sys
from collections import Counter, defaultdict

DEFAULT_SINCE_DAYS = 180
MAX_SINCE_DAYS = 3650
MIN_COMMITS = 30
MAX_POINTS = 15
MAX_PAIRS = 10
MAX_GROWTH = 10
MEASURE_LIMIT = 300
DIR_LABEL_SEGMENTS = 2
BIG_COMMIT_FILES = 30
MIN_SHARED = 5
MIN_COUPLING = 0.3
BINARY_SNIFF_BYTES = 8000
POINT_MIN = 0.05
POINT_MAX = 0.9
QUADRANT_SIZE_PX = 900
COMMIT_MARK = "__SMELL_COMMIT__"

EXCLUDED_DIRS = {"Intermediate", "Binaries", "ThirdParty", "node_modules", "synced"}
EXCLUDED_NAMES = ["*.generated.*", "*Generated.*", "*.lock", "package-lock.json", "pnpm-lock.yaml"]
BINARY_EXTENSIONS = {
    ".uasset", ".umap", ".ubulk", ".uexp", ".dll", ".exe", ".lib", ".a", ".so", ".dylib", ".pdb", ".obj", ".o",
    ".res", ".png", ".jpg", ".jpeg", ".tga", ".psd", ".fbx", ".wav", ".ogg", ".mp4", ".zip", ".7z", ".pak",
    ".xlsx", ".xlsm", ".xls",
}
# Data files churn with content edits, not design; noise for an architecture review unless asked for.
DATA_EXTENSIONS = {".csv", ".tsv", ".xml", ".xsd", ".archive", ".locres", ".manifest", ".po"}

CHANGED_COLOR = "#c08a00"


class SmellMapError(Exception):
    pass


def decode(raw):
    for enc in ("utf-8", "cp949"):
        try:
            return raw.decode(enc)
        except UnicodeDecodeError:
            continue
    return raw.decode("utf-8", errors="replace")


def git(root, args, stdin=None):
    cmd = ["git", "-c", "core.quotepath=false", "-C", root] + args
    proc = subprocess.run(cmd, input=stdin, capture_output=True)
    if proc.returncode != 0:
        raise SmellMapError("git %s failed: %s" % (args[0], decode(proc.stderr).strip()))
    return proc.stdout


def is_excluded(path, extra_patterns, include_data=False):
    parts = path.split("/")
    if any(p in EXCLUDED_DIRS for p in parts[:-1]):
        return True
    name = parts[-1]
    ext = posixpath.splitext(name)[1].lower()
    if ext in BINARY_EXTENSIONS:
        return True
    if not include_data and ext in DATA_EXTENSIONS:
        return True
    if any(fnmatch.fnmatch(name, pat) for pat in EXCLUDED_NAMES):
        return True
    return any(fnmatch.fnmatch(path, pat) for pat in extra_patterns)


def parse_log(text, extra_patterns, include_data=False):
    """Return a list of per-commit file lists from `git log --name-only` output."""
    commits = []
    current = None
    for line in text.splitlines():
        if line.startswith(COMMIT_MARK):
            current = []
            commits.append(current)
            continue
        path = line.strip()
        if current is None or not path:
            continue
        if not is_excluded(path, extra_patterns, include_data):
            current.append(path)
    return [files for files in commits if files]


def compute_churn(commits):
    churn = Counter()
    for files in commits:
        churn.update(set(files))
    return churn


def is_split_pair(a, b):
    """Same name, different extension (Foo.h / Foo.cpp): expected to change together."""
    stem_a, ext_a = posixpath.splitext(posixpath.basename(a))
    stem_b, ext_b = posixpath.splitext(posixpath.basename(b))
    return stem_a == stem_b and ext_a != ext_b


def compute_couplings(commits, churn):
    """Cross-boundary co-change pairs, strongest first."""
    shared = Counter()
    for files in commits:
        unique = sorted(set(files))
        if len(unique) > BIG_COMMIT_FILES:
            continue
        for a, b in itertools.combinations(unique, 2):
            if posixpath.dirname(a) == posixpath.dirname(b):
                continue
            if is_split_pair(a, b):
                continue
            shared[(a, b)] += 1
    pairs = []
    for (a, b), count in shared.items():
        if count < MIN_SHARED:
            continue
        degree = count / ((churn[a] + churn[b]) / 2.0)
        if degree < MIN_COUPLING:
            continue
        pairs.append({"a": a, "b": b, "shared": count, "degree": round(degree, 2)})
    pairs.sort(key=lambda p: (-p["shared"], -p["degree"], p["a"], p["b"]))
    return pairs[:MAX_PAIRS]


def count_lines(data):
    """Non-blank line count, or None when the content looks binary."""
    if b"\0" in data[:BINARY_SNIFF_BYTES]:
        return None
    return sum(1 for line in data.splitlines() if line.strip())


def working_tree_lines(root, paths, limit=MEASURE_LIMIT):
    """Sizes of the first `limit` text files in `paths` order; deleted and binary files are skipped."""
    sizes = {}
    for path in paths:
        if len(sizes) >= limit:
            break
        full = os.path.join(root, *path.split("/"))
        try:
            with open(full, "rb") as f:
                head = f.read(BINARY_SNIFF_BYTES)
                if b"\0" in head:
                    continue
                lines = count_lines(head + f.read())
        except OSError:
            continue
        if lines is not None:
            sizes[path] = lines
    return sizes


def blob_lines(root, rev, paths):
    """Line counts of `rev:path` blobs via one `git cat-file --batch`; absent paths are omitted."""
    if not rev or not paths:
        return {}
    request = "".join("%s:%s\n" % (rev, p) for p in paths).encode("utf-8")
    out = git(root, ["cat-file", "--batch"], stdin=request)
    sizes = {}
    pos = 0
    for path in paths:
        end = out.index(b"\n", pos)
        header = out[pos:end].split()
        pos = end + 1
        if len(header) < 3 or header[1] != b"blob":
            continue
        size = int(header[2])
        lines = count_lines(out[pos:pos + size])
        pos += size + 1
        if lines is not None:
            sizes[path] = lines
    return sizes


def normalize(value, low, high):
    """Log min-max within the plotted points; the top-ranked files all sit near the global max otherwise."""
    if high <= low:
        return 0.5
    return (math.log1p(value) - math.log1p(low)) / (math.log1p(high) - math.log1p(low))


def point(value, low, high):
    return round(min(POINT_MAX, max(POINT_MIN, normalize(value, low, high))), 3)


def short_names(paths):
    """Basename, or parent/basename where basenames collide, or the full path as last resort."""
    names = {}
    by_base = defaultdict(list)
    for p in paths:
        by_base[posixpath.basename(p)].append(p)
    for base, group in by_base.items():
        if len(group) == 1:
            names[group[0]] = base
            continue
        for p in group:
            parent = posixpath.basename(posixpath.dirname(p))
            names[p] = posixpath.join(parent, base) if parent else base
        if len({names[p] for p in group}) < len(group):
            for p in group:
                names[p] = p
    return names


def short_dir(directory):
    """Last two path segments, so subgraph titles do not overflow their boxes."""
    parts = [p for p in directory.split("/") if p]
    if not parts:
        return "/"
    if len(parts) <= DIR_LABEL_SEGMENTS:
        return "/".join(parts)
    return ".../" + "/".join(parts[-DIR_LABEL_SEGMENTS:])


def quote(text):
    return '"%s"' % text.replace('"', "'")


def compute_hotspots(churn, sizes):
    measured = [p for p in churn if p in sizes]
    if not measured:
        return []
    ranked = sorted(measured, key=lambda p: (-churn[p] * sizes[p], p))[:MAX_POINTS]
    churns = [churn[p] for p in ranked]
    lines = [sizes[p] for p in ranked]
    return [{"path": p, "commits": churn[p], "lines": sizes[p],
             "x": point(churn[p], min(churns), max(churns)),
             "y": point(sizes[p], min(lines), max(lines))} for p in ranked]


def compute_growth(head_sizes, base_sizes, touched, base_text_paths):
    """Net growth over the period; new files start at 0, files unreadable at the base are skipped."""
    rows = []
    for path in touched:
        if path not in head_sizes:
            continue
        if path in base_sizes:
            start = base_sizes[path]
        elif path in base_text_paths:
            continue
        else:
            start = 0
        delta = head_sizes[path] - start
        if delta > 0:
            rows.append({"path": path, "start": start, "end": head_sizes[path], "delta": delta})
    rows.sort(key=lambda r: (-r["delta"], r["path"]))
    return rows[:MAX_GROWTH]


def hotspot_diagram(hotspots, period):
    names = short_names([h["path"] for h in hotspots])
    lines = [
        "---",
        "config:",
        "  quadrantChart:",
        "    chartWidth: %d" % QUADRANT_SIZE_PX,
        "    chartHeight: %d" % QUADRANT_SIZE_PX,
        "---",
        "quadrantChart",
        "  title 핫스팟 - 최근 %d일" % period["days"],
        "  x-axis 수정 적음 --> 수정 잦음",
        "  y-axis 작음 --> 큼",
        "  quadrant-1 핫스팟 - 먼저 볼 곳",
        "  quadrant-2 크지만 안정",
        "  quadrant-3 건강",
        "  quadrant-4 작지만 자주 바뀜",
    ]
    for h in hotspots:
        lines.append("  %s: [%s, %s]" % (quote(names[h["path"]]), h["x"], h["y"]))
    return "\n".join(lines)


def coupling_diagram(pairs):
    paths = sorted({p["a"] for p in pairs} | {p["b"] for p in pairs})
    ids = {p: "n%d" % i for i, p in enumerate(paths)}
    by_dir = defaultdict(list)
    for p in paths:
        by_dir[posixpath.dirname(p)].append(p)
    lines = ["flowchart TD"]
    for i, (directory, members) in enumerate(sorted(by_dir.items())):
        lines.append("  subgraph d%d[%s]" % (i, quote(short_dir(directory))))
        for p in members:
            lines.append("    %s[%s]" % (ids[p], quote(posixpath.basename(p))))
        lines.append("  end")
    for pair in pairs:
        lines.append("  %s ---|%s| %s" % (
            ids[pair["a"]], quote("%d회, %d%%" % (pair["shared"], round(pair["degree"] * 100))), ids[pair["b"]]))
    return "\n".join(lines)


def growth_diagram(rows, period):
    names = short_names([r["path"] for r in rows])
    top = max(r["delta"] for r in rows)
    return "\n".join([
        "---",
        "config:",
        "  themeVariables:",
        "    xyChart:",
        '      plotColorPalette: "%s"' % CHANGED_COLOR,
        "---",
        "xychart-beta horizontal",
        "  title %s" % quote("순증가 줄 수 - 최근 %d일" % period["days"]),
        "  x-axis [%s]" % ", ".join(quote(names[r["path"]]) for r in rows),
        '  y-axis "lines" 0 --> %d' % top,
        "  bar [%s]" % ", ".join(str(r["delta"]) for r in rows),
    ])


def diagram(kind, title, caption, data, render, empty_reason):
    entry = {"kind": kind, "title": title, "caption": caption}
    if data:
        entry["mermaid"] = render()
    else:
        entry["empty_reason"] = empty_reason
    return entry


def read_history(root, since, pathspec, extra_patterns, include_data):
    args = ["log", "--name-only", "--no-renames", "--format=" + COMMIT_MARK + "%H",
            "--since=" + since.strftime("%Y-%m-%d %H:%M:%S")]
    if pathspec:
        args += ["--", pathspec]
    return parse_log(decode(git(root, args)), extra_patterns, include_data)


def total_commits(root, pathspec):
    args = ["rev-list", "--count", "HEAD"]
    if pathspec:
        args += ["--", pathspec]
    return int(git(root, args).strip() or 0)


def build(root, since_days, pathspec, extra_patterns, include_data=False, now=None):
    now = now or datetime.datetime.now()
    available = total_commits(root, pathspec)
    if available == 0:
        raise SmellMapError("no commits in %s%s" % (root, " for " + pathspec if pathspec else ""))
    days = since_days
    while True:
        since = now - datetime.timedelta(days=days)
        commits = read_history(root, since, pathspec, extra_patterns, include_data)
        if len(commits) >= MIN_COMMITS or days >= MAX_SINCE_DAYS or len(commits) >= available:
            break
        print("only %d commits in %d days, widening" % (len(commits), days), file=sys.stderr)
        days = min(days * 2, MAX_SINCE_DAYS)
    period = {"days": days, "since": since.strftime("%Y-%m-%d"), "commits": len(commits)}

    churn = compute_churn(commits)
    by_churn = sorted(churn, key=lambda p: (-churn[p], p))
    head_sizes = working_tree_lines(root, by_churn)
    touched = sorted(head_sizes)
    base_rev = git(root, ["rev-list", "-1", "--before=" + since.strftime("%Y-%m-%d %H:%M:%S"), "HEAD"]).strip()
    base_rev = decode(base_rev)
    base_sizes = blob_lines(root, base_rev, touched)
    base_present = set(blob_paths(root, base_rev, touched))

    hotspots = compute_hotspots(churn, head_sizes)
    couplings = compute_couplings(commits, churn)
    growth = compute_growth(head_sizes, base_sizes, touched, base_present)

    diagrams = [
        diagram("hotspot", "핫스팟 - 수정 빈도 x 크기",
                "x = 최근 %d일 수정 커밋 수, y = 빈 줄 제외 줄 수, 수정 수 x 줄 수 상위 %d개 안에서의 상대 위치(로그 스케일)"
                % (days, MAX_POINTS),
                hotspots, lambda: hotspot_diagram(hotspots, period), "측정 가능한 텍스트 파일 없음"),
        diagram("co-change", "함께 바뀌는 파일 - 경계를 넘는 결합",
                "다른 디렉터리끼리 %d회 이상, 결합도 %d%% 이상 함께 바뀐 쌍 (파일 %d개 초과 커밋 제외)"
                % (MIN_SHARED, round(MIN_COUPLING * 100), BIG_COMMIT_FILES),
                couplings, lambda: coupling_diagram(couplings), "기준을 넘는 쌍 없음"),
        diagram("growth", "비대화 - 기간 내 순증가",
                "%s 이후 늘어난 빈 줄 제외 줄 수, 수정 빈도 상위 %d개 텍스트 파일 중, 신규 파일은 0에서 시작"
                % (period["since"], MEASURE_LIMIT),
                growth, lambda: growth_diagram(growth, period), "늘어난 파일 없음"),
    ]
    return {"repo": root, "path": pathspec, "period": period, "hotspots": hotspots,
            "couplings": couplings, "growth": growth, "diagrams": diagrams}


def blob_paths(root, rev, paths):
    """Paths that exist at `rev` (text or not), so binary-at-base files are not mistaken for new ones."""
    if not rev or not paths:
        return []
    request = "".join("%s:%s\n" % (rev, p) for p in paths).encode("utf-8")
    out = git(root, ["cat-file", "--batch-check"], stdin=request)
    present = []
    for path, line in zip(paths, out.splitlines()):
        if line.split()[1:2] == [b"blob"]:
            present.append(path)
    return present


def resolve_root(repo):
    try:
        return decode(git(repo, ["rev-parse", "--show-toplevel"]).strip())
    except (SmellMapError, OSError) as e:
        raise SmellMapError("not a git repository: %s (%s)" % (repo, e))


def relative_pathspec(root, path):
    if not path:
        return None
    if os.path.isabs(path):
        path = os.path.relpath(path, root)
    return path.replace("\\", "/")


def main(argv=None):
    parser = argparse.ArgumentParser(description=__doc__.strip().splitlines()[0])
    parser.add_argument("--repo", default=os.getcwd())
    parser.add_argument("--since-days", type=int, default=DEFAULT_SINCE_DAYS)
    parser.add_argument("--path")
    parser.add_argument("--exclude", action="append", default=[])
    parser.add_argument("--include-data", action="store_true", help="keep data files (csv, xml, localization)")
    args = parser.parse_args(argv)
    if args.since_days <= 0:
        parser.error("--since-days must be positive")
    try:
        root = resolve_root(args.repo)
        result = build(root, args.since_days, relative_pathspec(root, args.path), args.exclude, args.include_data)
    except SmellMapError as e:
        print("smell_map: %s" % e, file=sys.stderr)
        return 1
    sys.stdout.reconfigure(encoding="utf-8")
    json.dump(result, sys.stdout, ensure_ascii=False, indent=2)
    sys.stdout.write("\n")
    return 0


if __name__ == "__main__":
    sys.exit(main())

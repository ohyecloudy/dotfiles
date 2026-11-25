"""Claude Code hook that keeps each file's line endings as they were before a Bash or Write call.

Write saves LF and Git Bash `sed -i` drops CR, so without this agents spend calls checking and
converting line endings, or leave whole-file EOL diffs behind.

Usage (settings.json hooks, matcher "Bash|Write"):
    PreToolUse:  python3 -X utf8 eol_guard.py pre
    PostToolUse: python3 -X utf8 eol_guard.py post

Contract: always exits 0 and never blocks the tool. Errors go to stderr only.
"""

import glob
import json
import os
import re
import subprocess
import sys
import tempfile

MAX_BYTES = 10 * 1024 * 1024
MAX_FILES = 200
STATE_DIR = os.path.join(tempfile.gettempdir(), "claude-eol-guard")
EXPLICIT_CONVERSION = re.compile(r"\bunix2dos\b|\bdos2unix\b|#\s*eol-change\b")
# Shell word characters that can be part of a path; quotes and operators split tokens.
TOKEN = re.compile(r"[^\s'\"`;|&<>(){}=,]+")
MSYS_DRIVE = re.compile(r"^/([A-Za-z])(/|$)")
GLOB_CHARS = re.compile(r"[*?\[]")

CRLF, LF, MIXED, NONE, SKIP = "crlf", "lf", "mixed", "none", "skip"
# Claude Code runs hooks with MSYS python, which reports 'cygwin' and LF but still edits Windows files.
IS_WINDOWS = os.name == "nt" or sys.platform in ("cygwin", "msys")
NATIVE_EOL = CRLF if IS_WINDOWS else LF


def classify(data):
    if b"\0" in data:
        return SKIP
    crlf = data.count(b"\r\n")
    lf = data.count(b"\n") - crlf
    if crlf and lf:
        return MIXED
    if crlf:
        return CRLF
    if lf:
        return LF
    return NONE


def file_eol(path):
    try:
        if os.path.getsize(path) > MAX_BYTES:
            return SKIP
        with open(path, "rb") as f:
            return classify(f.read())
    except OSError:
        return SKIP


def convert(path, target):
    with open(path, "rb") as f:
        data = f.read()
    data = data.replace(b"\r\n", b"\n")
    if target == CRLF:
        data = data.replace(b"\n", b"\r\n")
    with open(path, "wb") as f:
        f.write(data)


class PathResolver:
    """Turns shell words into native paths; native python cannot open MSYS paths like /c/x or /tmp/x."""

    def __init__(self, cwd):
        self.cwd = cwd
        self._msys_root = None

    def msys_root(self):
        if self._msys_root is None:
            try:
                out = subprocess.run(["cygpath", "-m", "/"], capture_output=True, text=True, check=True)
                self._msys_root = out.stdout.strip().rstrip("/")
            except (OSError, subprocess.CalledProcessError):
                self._msys_root = ""
        return self._msys_root

    def resolve(self, word):
        if word.startswith("~"):
            word = os.path.expanduser(word)
        if os.name == "nt":
            m = MSYS_DRIVE.match(word)
            if m:
                word = m.group(1).upper() + ":/" + word[m.end():]
            elif word.startswith("/") and self.msys_root():
                word = self.msys_root() + word
        if not os.path.isabs(word):
            word = os.path.join(self.cwd, word)
        return os.path.normpath(word)


def bash_candidates(command, cwd):
    resolver = PathResolver(cwd)
    found = []
    seen = set()
    for word in TOKEN.findall(command):
        if word.startswith("-") or not re.search(r"[./\\~]", word):
            continue
        try:
            path = resolver.resolve(word)
        except (OSError, ValueError):
            continue
        paths = glob.glob(path) if GLOB_CHARS.search(path) else [path]
        for p in paths:
            if p not in seen and os.path.isfile(p):
                seen.add(p)
                found.append(p)
                if len(found) >= MAX_FILES:
                    return found
    return found


def state_path(tool_use_id):
    safe = re.sub(r"[^A-Za-z0-9_-]", "_", tool_use_id or "unknown")
    return os.path.join(STATE_DIR, safe + ".json")


def write_file_path(payload):
    path = payload["tool_input"]["file_path"]
    if not os.path.isabs(path):
        path = os.path.join(payload.get("cwd") or os.getcwd(), path)
    return os.path.normpath(path)


def pre(payload):
    tool = payload.get("tool_name")
    if tool == "Write":
        path = write_file_path(payload)
        state = {path: file_eol(path) if os.path.isfile(path) else "new"}
    elif tool == "Bash":
        command = payload["tool_input"].get("command", "")
        if EXPLICIT_CONVERSION.search(command):
            return None
        cwd = payload.get("cwd") or os.getcwd()
        state = {p: file_eol(p) for p in bash_candidates(command, cwd)}
    else:
        return None
    state = {p: e for p, e in state.items() if e in (CRLF, LF, "new")}
    if state:
        os.makedirs(STATE_DIR, exist_ok=True)
        with open(state_path(payload.get("tool_use_id")), "w", encoding="utf-8") as f:
            json.dump(state, f)
    return None


def new_file_eol(path):
    """Line ending for a new file: .gitattributes eol, else the platform default inside a repository.

    None outside a repository (or when git fails): no convention to follow, so the file stays as written.
    """
    try:
        out = subprocess.run(
            ["git", "-C", os.path.dirname(path), "check-attr", "eol", "--", os.path.basename(path)],
            capture_output=True, text=True, check=True,
        ).stdout.strip()
    except (OSError, subprocess.CalledProcessError):
        return None
    value = out.rsplit(": ", 1)[-1]
    return value if value in (CRLF, LF) else NATIVE_EOL


def post(payload):
    sp = state_path(payload.get("tool_use_id"))
    try:
        with open(sp, encoding="utf-8") as f:
            state = json.load(f)
    except OSError:
        return None
    try:
        os.remove(sp)
    except OSError:
        pass

    restored = []
    for path, before in state.items():
        if not os.path.isfile(path):
            continue
        target = new_file_eol(path) if before == "new" else before
        now = file_eol(path)
        if target is None or now in (SKIP, NONE) or now == target:
            continue
        convert(path, target)
        restored.append("%s -> %s" % (path.replace("\\", "/"), target.upper()))

    if not restored:
        return None
    return {
        "hookSpecificOutput": {
            "hookEventName": "PostToolUse",
            "additionalContext": "eol_guard: line endings restored (no need to convert again): "
            + "; ".join(restored),
        }
    }


def main(argv):
    if not IS_WINDOWS and not os.environ.get("EOL_GUARD_FORCE"):
        return 0
    try:
        payload = json.loads(sys.stdin.buffer.read().decode("utf-8"))
        result = {"pre": pre, "post": post}[argv[1]](payload)
        if result:
            sys.stdout.write(json.dumps(result, ensure_ascii=False))
    except Exception as e:  # never block the tool because of this hook
        print("eol_guard: %s: %s" % (type(e).__name__, e), file=sys.stderr)
    return 0


if __name__ == "__main__":
    sys.exit(main(sys.argv))

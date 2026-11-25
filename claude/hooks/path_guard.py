"""Claude Code hook that blocks every tool call touching private directories under ~/org_roam.

Read deny rules in settings.json only cover the built-in file tools; Bash (cat, grep, python ...)
bypasses them and native Windows has no sandbox. This hook closes that gap by matching the
directory names in the tool input, so every path spelling (~/, /c/, C:/, C:\\) is caught.

Usage (settings.json hooks):
    PreToolUse, matcher "Bash|PowerShell|Read|Grep|Glob": python3 -X utf8 path_guard.py

Contract: exit 2 (block, reason on stderr) when any string in tool_input refers to a protected
directory, or when stdin is not a valid hook payload (fail closed). Otherwise exit 0.
Best effort: paths built at runtime (variables, generated scripts) are not detected.
"""

import json
import re
import sys

ROOT = "org_roam"
PROTECTED = ("contact", "daily", "weekly")

EXIT_ALLOW = 0
EXIT_BLOCK = 2

_NAMES = "|".join(PROTECTED)
# Direct path: org_roam/daily, org_roam\\daily, ...
DIRECT = re.compile(rf"{ROOT}[\\/]+(?:{_NAMES})(?![\w.-])", re.IGNORECASE)
ROOT_WORD = re.compile(rf"(?<![\w.-]){ROOT}(?![\w.-])", re.IGNORECASE)
# Relative use after `cd ~/org_roam`: a bare protected name or one followed by a separator.
NAME_WORD = re.compile(rf"(?<![\w.-])(?:{_NAMES})(?![\w.-]|-)", re.IGNORECASE)


def strings_in(value):
    if isinstance(value, str):
        yield value
    elif isinstance(value, dict):
        for item in value.values():
            yield from strings_in(item)
    elif isinstance(value, list):
        for item in value:
            yield from strings_in(item)


def touches_protected(tool_input):
    texts = list(strings_in(tool_input))
    if any(DIRECT.search(text) for text in texts):
        return True
    joined = "\n".join(texts)
    return bool(ROOT_WORD.search(joined) and NAME_WORD.search(joined))


def main():
    try:
        payload = json.load(sys.stdin)
        tool_input = payload["tool_input"]
    except (ValueError, KeyError, TypeError) as error:
        print(f"path_guard: invalid hook payload, blocking: {error}", file=sys.stderr)
        return EXIT_BLOCK

    if touches_protected(tool_input):
        names = ", ".join(f"~/{ROOT}/{name}" for name in PROTECTED)
        print(f"path_guard: access to {names} is not allowed. Do not retry another way.",
              file=sys.stderr)
        return EXIT_BLOCK
    return EXIT_ALLOW


if __name__ == "__main__":
    sys.exit(main())

"""Claude Code Stop hook that sends a reply back for rewriting when it breaks the CLAUDE.md notation rules.

The rules (ASCII symbols only, Korean replies, Windows code locations that Emacs gF can open) are
broken often even right after they are written down, so a machine check replaces the reminder.

Usage (settings.json hooks):
    Stop: python3 -X utf8 reply_style_guard.py

Contract: always exits 0. On a violation prints {"decision": "block", "reason": ...} so Claude
continues and rewrites the reply. Never blocks twice in a row (stop_hook_active) and lets the reply
through when the payload has no last_assistant_message.
"""

import json
import re
import sys

FORBIDDEN_SYMBOLS = {
    "\u2192": "->", "\u2190": "<-", "\u2194": "<->", "\u21d2": "=>", "\u21d0": "<=",
    "\u2014": "-", "\u2013": "-", "\u2026": "...", "\u00b7": ", or /", "\u2022": "-",
    "\u2248": "~", "\u00d7": "x", "\u2260": "!=", "\u2264": "<=", "\u2265": ">=",
    "\u201c": '"', "\u201d": '"', "\u2018": "'", "\u2019": "'",
}
# Code locations (path:line) in MSYS (/c/Users) or URL style (/c:/Users, /D:/work); Emacs gF cannot
# open them. Paths without a line, such as commands or format examples, are not code locations.
MSYS_LOCATION = re.compile(r"(?<![\w.~/:-])/[A-Za-z]:?/[^\s`'\"()<>]*?:\d+")
FENCED_CODE = re.compile(r"^[ \t]*(```|~~~).*?^[ \t]*\1[^\n]*$", re.MULTILINE | re.DOTALL)
INLINE_CODE = re.compile(r"`[^`\n]+`")
HANGUL = re.compile(r"[\uac00-\ud7a3]")
LATIN_LETTER = re.compile(r"[A-Za-z]")
# Short replies such as "OK" or a commit hash need no Korean.
MIN_LATIN_LETTERS_FOR_LANGUAGE_CHECK = 40
MAX_EXAMPLES = 3


def violations(reply):
    outside_fences = FENCED_CODE.sub("", reply)
    prose = INLINE_CODE.sub("", outside_fences)
    found = []

    symbols = sorted({ch for ch in prose if ch in FORBIDDEN_SYMBOLS})
    if symbols:
        pairs = ", ".join(f"'{ch}' -> '{FORBIDDEN_SYMBOLS[ch]}'" for ch in symbols)
        found.append(f"non-ASCII symbols outside code: {pairs}")

    locations = MSYS_LOCATION.findall(outside_fences)[:MAX_EXAMPLES]
    if locations:
        found.append(f"MSYS or /X:/ style code locations ({', '.join(locations)}); "
                     "write them as C:/...:line for Emacs gF")

    if not HANGUL.search(prose) and len(LATIN_LETTER.findall(prose)) >= MIN_LATIN_LETTERS_FOR_LANGUAGE_CHECK:
        found.append("reply is not in Korean")
    return found


def main():
    try:
        payload = json.load(sys.stdin)
    except ValueError as error:
        print(f"reply_style_guard: invalid hook payload: {error}", file=sys.stderr)
        return 0

    reply = payload.get("last_assistant_message")
    if payload.get("stop_hook_active") or not isinstance(reply, str):
        return 0

    found = violations(reply)
    if found:
        reason = ("The last reply breaks the CLAUDE.md notation rules. Send the corrected reply again "
                  "in full, without commenting on the fix:\n- " + "\n- ".join(found))
        print(json.dumps({"decision": "block", "reason": reason}))
    return 0


if __name__ == "__main__":
    sys.exit(main())

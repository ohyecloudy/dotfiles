"""Behavior tests for reply_style_guard.py, driven through its stdin hook protocol like Claude Code does."""

import json
import os
import subprocess
import sys
import unittest

HOOK = os.path.join(os.path.dirname(os.path.abspath(__file__)), "reply_style_guard.py")
KOREAN = "변경 완료. "


def run_hook(payload):
    proc = subprocess.run(
        [sys.executable, "-X", "utf8", HOOK],
        input=json.dumps(payload).encode("utf-8"),
        capture_output=True,
        check=False,
    )
    assert proc.returncode == 0, proc.stderr.decode("utf-8", "replace")
    out = proc.stdout.decode("utf-8").strip()
    return json.loads(out) if out else None


def check(reply, active=False):
    return run_hook({"hook_event_name": "Stop", "stop_hook_active": active, "last_assistant_message": reply})


class Blocks(unittest.TestCase):
    def test_symbols_in_prose(self):
        for reply in (KOREAN + "A \u2192 B", KOREAN + "\u2014 note", KOREAN + "wait\u2026", KOREAN + "\u201cq\u201d"):
            with self.subTest(reply=reply):
                result = check(reply)
                self.assertEqual(result["decision"], "block")
                self.assertIn("non-ASCII", result["reason"])

    def test_msys_and_url_style_paths(self):
        for path in ("/c/Users/me/a.py:3", "/c:/Users/me/a.py:3", "/D:/work/x.cpp:10"):
            with self.subTest(path=path):
                result = check(f"{KOREAN}`{path}` 수정")
                self.assertIn(path.split(":")[0][:3], result["reason"])

    def test_english_reply(self):
        result = check("The Excel tab search box is done and committed as a single change.")
        self.assertIn("not in Korean", result["reason"])


class Allows(unittest.TestCase):
    def test_clean_korean_reply(self):
        self.assertIsNone(check(KOREAN + "`C:/Users/me/a.py:3` -> 수정, https://example.com/a/b/c 참고"))

    def test_code_is_exempt_from_symbols_and_language(self):
        reply = KOREAN + "`\u2192` 금지\n```\nprint('\u2192 English text inside a fenced block only here')\ncd /c/Users/me\n```\n"
        self.assertIsNone(check(reply))

    def test_short_english_and_unix_like_text(self):
        self.assertIsNone(check("OK, done."))
        self.assertIsNone(check(KOREAN + "and/or, s/a/b/, ~/x/y, ./a/b"))

    def test_second_stop_and_missing_message(self):
        self.assertIsNone(check(KOREAN + "\u2192", active=True))
        self.assertIsNone(run_hook({"hook_event_name": "Stop"}))


if __name__ == "__main__":
    unittest.main()

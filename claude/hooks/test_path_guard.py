"""Behavior tests for path_guard.py, driven through its stdin hook protocol like Claude Code does."""

import json
import os
import subprocess
import sys
import unittest

HOOK = os.path.join(os.path.dirname(os.path.abspath(__file__)), "path_guard.py")


def run_hook(raw):
    proc = subprocess.run(
        [sys.executable, "-X", "utf8", HOOK],
        input=raw.encode("utf-8"),
        capture_output=True,
        check=False,
    )
    return proc.returncode


def run_tool(tool_name, tool_input):
    return run_hook(json.dumps({"tool_name": tool_name, "tool_input": tool_input}))


class Blocks(unittest.TestCase):
    def test_path_spellings(self):
        for command in (
            "cat ~/org_roam/daily/2026-11-25.org",
            "grep -r foo /c/Users/me/org_roam/contact",
            "type C:\\Users\\me\\org_roam\\weekly\\w48.org",
            "ls C:/Users/me/ORG_ROAM/Daily",
            "python -c \"open('C:\\\\Users\\\\me\\\\org_roam\\\\daily\\\\a.org')\"",
        ):
            with self.subTest(command=command):
                self.assertEqual(run_tool("Bash", {"command": command}), 2)

    def test_relative_after_cd(self):
        self.assertEqual(run_tool("Bash", {"command": "cd ~/org_roam && cat daily/a.org"}), 2)

    def test_split_across_fields(self):
        self.assertEqual(run_tool("Grep", {"pattern": "x", "path": "~/org_roam", "glob": "daily/**"}), 2)
        self.assertEqual(run_tool("Read", {"file_path": "C:/Users/me/org_roam/daily/a.org"}), 2)

    def test_invalid_payload_fails_closed(self):
        self.assertEqual(run_hook("not json"), 2)
        self.assertEqual(run_hook("{}"), 2)


class Allows(unittest.TestCase):
    def test_other_org_roam_files(self):
        for command in (
            "cat ~/org_roam/notes/daily-standup.org",
            "cat ~/org_roam/20261125-dailyish.org",
            "ls ~/org_roam",
        ):
            with self.subTest(command=command):
                self.assertEqual(run_tool("Bash", {"command": command}), 0)

    def test_unrelated(self):
        self.assertEqual(run_tool("Bash", {"command": "grep daily notes.txt"}), 0)
        self.assertEqual(run_tool("Glob", {"pattern": "**/*.py"}), 0)


if __name__ == "__main__":
    unittest.main()

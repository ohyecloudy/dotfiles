"""Behavior tests for eol_guard.py, driven through its stdin hook protocol like Claude Code does."""

import json
import os
import shutil
import subprocess
import sys
import tempfile
import unittest

HOOK = os.path.join(os.path.dirname(os.path.abspath(__file__)), "eol_guard.py")
CRLF_TEXT = b"a\r\nb\r\nc\r\n"
LF_TEXT = b"a\nb\nc\n"


def run_hook(mode, payload):
    env = dict(os.environ, EOL_GUARD_FORCE="1")
    proc = subprocess.run(
        [sys.executable, "-X", "utf8", HOOK, mode],
        input=json.dumps(payload).encode("utf-8"),
        capture_output=True,
        env=env,
        check=False,
    )
    assert proc.returncode == 0, proc.stderr.decode("utf-8", "replace")
    out = proc.stdout.decode("utf-8").strip()
    return json.loads(out) if out else None


class Case(unittest.TestCase):
    def setUp(self):
        self.dir = tempfile.mkdtemp(prefix="eolguard-test-")
        self.counter = 0

    def tearDown(self):
        shutil.rmtree(self.dir, ignore_errors=True)

    def path(self, name):
        return os.path.join(self.dir, name).replace("\\", "/")

    def put(self, name, data):
        with open(self.path(name), "wb") as f:
            f.write(data)
        return self.path(name)

    def read(self, name):
        with open(self.path(name), "rb") as f:
            return f.read()

    def bash(self, command, edit):
        """Run pre, apply EDIT (simulated tool effect), run post. Returns post output."""
        self.counter += 1
        payload = {
            "tool_name": "Bash",
            "tool_use_id": "t%d" % self.counter,
            "cwd": self.dir,
            "tool_input": {"command": command},
        }
        run_hook("pre", payload)
        edit()
        return run_hook("post", payload)

    def write(self, name, data):
        self.counter += 1
        payload = {
            "tool_name": "Write",
            "tool_use_id": "w%d" % self.counter,
            "cwd": self.dir,
            "tool_input": {"file_path": self.path(name), "content": data.decode("utf-8")},
        }
        run_hook("pre", payload)
        self.put(name, data)
        return run_hook("post", payload)


class WriteTests(Case):
    def test_new_file_outside_git_is_left_as_written(self):
        self.write("plan.md", LF_TEXT)
        self.assertEqual(self.read("plan.md"), LF_TEXT)

    def test_new_file_in_git_without_eol_attr_uses_os_line_ending(self):
        subprocess.run(["git", "init", "-q", self.dir], check=True)
        self.write("new.cpp", LF_TEXT)
        expected = CRLF_TEXT if os.linesep == "\r\n" else LF_TEXT
        self.assertEqual(self.read("new.cpp"), expected)

    def test_new_file_follows_gitattributes_eol(self):
        subprocess.run(["git", "init", "-q", self.dir], check=True)
        self.put(".gitattributes", b"*.sh eol=lf\n*.bat eol=crlf\n")
        self.write("run.sh", CRLF_TEXT)
        self.write("run.bat", LF_TEXT)
        self.assertEqual(self.read("run.sh"), LF_TEXT)
        self.assertEqual(self.read("run.bat"), CRLF_TEXT)

    def test_overwrite_keeps_previous_line_ending(self):
        self.put("old.h", CRLF_TEXT)
        out = self.write("old.h", b"x\ny\n")
        self.assertEqual(self.read("old.h"), b"x\r\ny\r\n")
        self.assertIn("old.h", out["hookSpecificOutput"]["additionalContext"])


class BashTests(Case):
    def test_crlf_file_turned_lf_is_restored_and_reported(self):
        f = self.put("a.cpp", CRLF_TEXT)
        out = self.bash("sed -i 's/a/z/' %s" % f, lambda: self.put("a.cpp", b"z\nb\nc\n"))
        self.assertEqual(self.read("a.cpp"), b"z\r\nb\r\nc\r\n")
        ctx = out["hookSpecificOutput"]
        self.assertEqual(ctx["hookEventName"], "PostToolUse")
        self.assertIn("a.cpp", ctx["additionalContext"])

    def test_lf_file_turned_crlf_is_restored(self):
        f = self.put("t.json", LF_TEXT)
        self.bash("python fix.py %s" % f, lambda: self.put("t.json", CRLF_TEXT))
        self.assertEqual(self.read("t.json"), LF_TEXT)

    def test_partially_mixed_after_edit_is_restored(self):
        f = self.put("p.org", CRLF_TEXT)
        self.bash("perl -0pi -e 's/b/x\\ny/' %s" % f, lambda: self.put("p.org", b"a\r\nx\ny\r\nc\r\n"))
        self.assertEqual(self.read("p.org"), b"a\r\nx\r\ny\r\nc\r\n")

    def test_unchanged_line_ending_is_silent(self):
        f = self.put("u.cpp", CRLF_TEXT)
        out = self.bash("sed -i 's/a/z/' %s" % f, lambda: self.put("u.cpp", b"z\r\nb\r\nc\r\n"))
        self.assertIsNone(out)

    def test_explicit_conversion_is_not_reverted(self):
        f = self.put("d.txt", CRLF_TEXT)
        self.bash("dos2unix %s" % f, lambda: self.put("d.txt", LF_TEXT))
        self.assertEqual(self.read("d.txt"), LF_TEXT)
        g = self.put("e.txt", LF_TEXT)
        self.bash("sed -i 's/\\r*$/\\r/' %s # eol-change" % g, lambda: self.put("e.txt", CRLF_TEXT))
        self.assertEqual(self.read("e.txt"), CRLF_TEXT)

    def test_file_mixed_before_edit_is_left_alone(self):
        f = self.put("m.md", b"a\r\nb\nc\r\n")
        self.bash("sed -i 's/a/z/' %s" % f, lambda: self.put("m.md", b"z\nb\nc\n"))
        self.assertEqual(self.read("m.md"), b"z\nb\nc\n")

    def test_binary_file_is_left_alone(self):
        f = self.put("b.bin", b"\x00\r\n\x01\r\n")
        self.bash("tool %s" % f, lambda: self.put("b.bin", b"\x00\n\x01\n"))
        self.assertEqual(self.read("b.bin"), b"\x00\n\x01\n")

    def test_relative_path_and_glob_and_heredoc(self):
        self.put("r1.cs", CRLF_TEXT)
        self.put("g1.h", CRLF_TEXT)
        self.put("g2.h", CRLF_TEXT)
        self.put("h.cs", CRLF_TEXT)

        def edit():
            for n in ("r1.cs", "g1.h", "g2.h", "h.cs"):
                self.put(n, LF_TEXT)

        command = "sed -i 's/a/b/' r1.cs *.h\npython - <<'EOF'\nopen('h.cs')\nEOF"
        self.bash(command, edit)
        for n in ("r1.cs", "g1.h", "g2.h", "h.cs"):
            self.assertEqual(self.read(n), CRLF_TEXT, n)

    @unittest.skipUnless(os.name == "nt", "MSYS drive paths exist only on Windows")
    def test_msys_drive_path(self):
        f = self.put("w.cpp", CRLF_TEXT)
        drive, rest = f[0].lower(), f[2:]
        self.bash("sed -i 's/a/z/' /%s%s" % (drive, rest), lambda: self.put("w.cpp", LF_TEXT))
        self.assertEqual(self.read("w.cpp"), CRLF_TEXT)


MSYS_PYTHON = "/usr/bin/python3" if os.name != "nt" else None
for _candidate in ("C:/git-sdk-64/usr/bin/python3.exe", "C:/msys64/usr/bin/python3.exe"):
    if os.name == "nt" and os.path.isfile(_candidate):
        MSYS_PYTHON = _candidate
        break


@unittest.skipUnless(os.name == "nt" and MSYS_PYTHON, "Claude Code runs hooks with MSYS python on Windows")
class MsysPythonTests(Case):
    """MSYS python reports sys.platform 'cygwin' and os.linesep LF, yet it runs on Windows."""

    def run_msys(self, mode, payload):
        env = {k: v for k, v in os.environ.items() if k != "EOL_GUARD_FORCE"}
        proc = subprocess.run(
            [MSYS_PYTHON, "-X", "utf8", os.path.basename(HOOK), mode],
            input=json.dumps(payload).encode("utf-8"),
            capture_output=True,
            env=env,
            cwd=os.path.dirname(HOOK),
            check=False,
        )
        self.assertEqual(proc.returncode, 0, proc.stderr.decode("utf-8", "replace"))

    def test_bash_restore_and_new_file_crlf(self):
        f = self.put("m.cpp", CRLF_TEXT)
        p = {"tool_name": "Bash", "tool_use_id": "m1", "cwd": self.dir,
             "tool_input": {"command": "sed -i 's/a/z/' %s" % f}}
        self.run_msys("pre", p)
        self.put("m.cpp", LF_TEXT)
        self.run_msys("post", p)
        self.assertEqual(self.read("m.cpp"), CRLF_TEXT)

        subprocess.run(["git", "init", "-q", self.dir], check=True)
        w = {"tool_name": "Write", "tool_use_id": "m2", "cwd": self.dir,
             "tool_input": {"file_path": self.path("n.cpp"), "content": "a\n"}}
        self.run_msys("pre", w)
        self.put("n.cpp", LF_TEXT)
        self.run_msys("post", w)
        self.assertEqual(self.read("n.cpp"), CRLF_TEXT)


if __name__ == "__main__":
    unittest.main()

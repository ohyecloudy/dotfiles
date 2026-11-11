"""Pins the smell map's filtering rules so the report's evidence stays comparable across runs."""

import datetime
import os
import shutil
import subprocess
import sys
import tempfile
import unittest

sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))

import smell_map  # noqa: E402


def log_text(*commits):
    out = []
    for files in commits:
        out.append(smell_map.COMMIT_MARK + "deadbeef")
        out.append("")
        out.extend(files)
    return "\n".join(out)


class ParseLogTest(unittest.TestCase):
    def test_excludes_generated_vendor_and_lock_files(self):
        commits = smell_map.parse_log(log_text([
            "src/a.cpp",
            "Intermediate/Build/x.cpp",
            "src/Foo.generated.h",
            "web/package-lock.json",
            "lib/ThirdParty/zlib.c",
        ]), [])
        self.assertEqual(commits, [["src/a.cpp"]])

    def test_extra_exclude_pattern_and_binary_extensions(self):
        commits = smell_map.parse_log(log_text(["Content/Hero.uasset", "Bin/x.DLL", "docs/a.md", "src/b.c"]), ["docs/*"])
        self.assertEqual(commits, [["src/b.c"]])

    def test_data_files_and_generated_sources_are_excluded_unless_asked(self):
        text = log_text(["Design/Npc.csv", "Info/Npc.xml", "Src/InfoGenerated.cpp", "Src/a.cpp"])
        self.assertEqual(smell_map.parse_log(text, []), [["Src/a.cpp"]])
        self.assertEqual(smell_map.parse_log(text, [], include_data=True), [["Design/Npc.csv", "Info/Npc.xml", "Src/a.cpp"]])


class CouplingTest(unittest.TestCase):
    def couplings(self, commits):
        return smell_map.compute_couplings(commits, smell_map.compute_churn(commits))

    def test_big_commits_do_not_create_pairs(self):
        big = ["x/a.c", "y/b.c"] + ["z/f%d.c" % i for i in range(smell_map.BIG_COMMIT_FILES)]
        self.assertEqual(self.couplings([big] * smell_map.MIN_SHARED), [])

    def test_same_directory_pairs_are_ignored(self):
        self.assertEqual(self.couplings([["src/a.c", "src/b.c"]] * smell_map.MIN_SHARED), [])

    def test_same_stem_pairs_across_directories_are_ignored(self):
        self.assertEqual(self.couplings([["Public/Foo.h", "Private/Foo.cpp"]] * smell_map.MIN_SHARED), [])

    def test_same_file_name_in_different_directories_is_coupling(self):
        found = self.couplings([["walkthrough/SKILL.md", "warm-start/SKILL.md"]] * smell_map.MIN_SHARED)
        self.assertEqual(len(found), 1)

    def test_cross_boundary_pair_needs_min_shared(self):
        pair = ["x/a.c", "y/b.c"]
        self.assertEqual(self.couplings([pair] * (smell_map.MIN_SHARED - 1)), [])
        found = self.couplings([pair] * smell_map.MIN_SHARED)
        self.assertEqual(found, [{"a": "x/a.c", "b": "y/b.c", "shared": smell_map.MIN_SHARED, "degree": 1.0}])

    def test_coupling_degree_threshold(self):
        shared = smell_map.MIN_SHARED
        # degree = shared / mean(churn); a alone 'solo' times pushes it below the threshold
        solo_below = int(2 * shared / smell_map.MIN_COUPLING) - 2 * shared + 2
        solo_at = int(2 * shared / smell_map.MIN_COUPLING) - 2 * shared
        base = [["x/a.c", "y/b.c"]] * shared
        self.assertEqual(self.couplings(base + [["x/a.c"]] * solo_below), [])
        self.assertEqual(len(self.couplings(base + [["x/a.c"]] * solo_at)), 1)


class HotspotTest(unittest.TestCase):
    def test_points_are_inside_unit_square_and_ranked(self):
        churn = {"a.c": 50, "b.c": 1, "c.c": 3}
        sizes = {"a.c": 2000, "b.c": 5, "c.c": 300}
        spots = smell_map.compute_hotspots(churn, sizes)
        self.assertEqual([s["path"] for s in spots], ["a.c", "c.c", "b.c"])
        for s in spots:
            self.assertTrue(0.0 <= s["x"] <= 1.0 and 0.0 <= s["y"] <= 1.0)

    def test_points_spread_across_plotted_range(self):
        churn = {"a.c": 90, "b.c": 80, "c.c": 70}
        sizes = {"a.c": 9000, "b.c": 8000, "c.c": 7000}
        xs = [s["x"] for s in smell_map.compute_hotspots(churn, sizes)]
        self.assertEqual((max(xs), min(xs)), (smell_map.POINT_MAX, smell_map.POINT_MIN))

    def test_unmeasured_files_are_skipped(self):
        self.assertEqual(smell_map.compute_hotspots({"bin.uasset": 9}, {}), [])

    def test_quadrant_labels_are_quoted(self):
        spots = smell_map.compute_hotspots({"x/warm-start.md": 3}, {"x/warm-start.md": 10})
        source = smell_map.hotspot_diagram(spots, {"days": 180})
        self.assertIn('"warm-start.md": [', source)


class ShortNamesTest(unittest.TestCase):
    def test_collisions_get_parent_directory(self):
        names = smell_map.short_names(["a/x/SKILL.md", "a/y/SKILL.md", "b/z.c"])
        self.assertEqual(names, {"a/x/SKILL.md": "x/SKILL.md", "a/y/SKILL.md": "y/SKILL.md", "b/z.c": "z.c"})

    def test_directory_labels_keep_last_two_segments(self):
        self.assertEqual(smell_map.short_dir("Windi/ContentTool/CSShare"), ".../ContentTool/CSShare")
        self.assertEqual(smell_map.short_dir("src/x"), "src/x")
        self.assertEqual(smell_map.short_dir(""), "/")


class GitRepoTest(unittest.TestCase):
    """End-to-end on a throwaway repository: period start, new files, growth."""

    def setUp(self):
        self.root = tempfile.mkdtemp()
        self.git("init", "-q")
        self.git("config", "user.email", "t@example.com")
        self.git("config", "user.name", "t")

    def tearDown(self):
        shutil.rmtree(self.root, ignore_errors=True)

    def git(self, *args, date=None):
        env = dict(os.environ)
        if date:
            env["GIT_AUTHOR_DATE"] = env["GIT_COMMITTER_DATE"] = date
        subprocess.run(["git", "-C", self.root] + list(args), check=True, capture_output=True, env=env)

    def write(self, path, lines):
        full = os.path.join(self.root, path)
        os.makedirs(os.path.dirname(full), exist_ok=True)
        with open(full, "w", encoding="utf-8") as f:
            f.write("".join("line %d\n" % i for i in range(lines)))

    def commit(self, date):
        self.git("add", "-A")
        self.git("commit", "-q", "-m", "c", date=date)

    def test_growth_counts_new_files_from_zero(self):
        now = datetime.datetime(2026, 11, 1, 12, 0, 0)
        self.write("src/old.c", 10)
        self.commit("2026-01-01T12:00:00")
        recent = smell_map.MIN_COMMITS
        for i in range(recent):
            self.write("src/old.c", 10 + i + 1)
            self.write("lib/new.c", i + 1)
            self.commit((now - datetime.timedelta(days=1, minutes=i)).strftime("%Y-%m-%dT%H:%M:%S"))
        result = smell_map.build(self.root, 30, None, [], now=now)
        self.assertEqual(result["period"], {"days": 30, "since": "2026-10-02", "commits": recent})
        growth = {g["path"]: g for g in result["growth"]}
        self.assertEqual((growth["lib/new.c"]["start"], growth["lib/new.c"]["delta"]), (0, recent))
        self.assertEqual((growth["src/old.c"]["start"], growth["src/old.c"]["delta"]), (10, recent))

    def test_period_widens_when_too_few_commits(self):
        now = datetime.datetime(2026, 11, 1, 12, 0, 0)
        self.write("src/old.c", 10)
        self.commit("2026-01-01T12:00:00")
        self.write("src/old.c", 20)
        self.commit("2026-10-30T12:00:00")
        result = smell_map.build(self.root, 30, None, [], now=now)
        self.assertEqual(result["period"]["commits"], 2)
        self.assertGreater(result["period"]["days"], 30)

    def test_no_commits_is_an_error(self):
        with self.assertRaises(smell_map.SmellMapError):
            smell_map.build(self.root, 30, None, [])


if __name__ == "__main__":
    unittest.main()

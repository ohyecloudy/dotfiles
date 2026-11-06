;;; my-ox-gfm-link-test.el --- Tests for my-ox-gfm-link  -*- lexical-binding: t; -*-

(require 'ert)
(require 'ox-gfm)
(require 'my-ox-gfm-link)

(defconst my/ox-gfm-link-test--prefix "https://atlassian.net/")

(defun my/ox-gfm-link-test--export (org-text &optional backend)
  "Export ORG-TEXT body with BACKEND (default `gfm'), return trimmed output."
  (let ((my/ox-gfm-link-url-only-prefixes (list my/ox-gfm-link-test--prefix))
        (org-export-with-toc nil))
    (string-trim (org-export-string-as org-text (or backend 'gfm) t))))

;;; Prefix matching

(ert-deftest my/ox-gfm-link-test/url-only-p-empty-prefixes ()
  (let ((my/ox-gfm-link-url-only-prefixes '()))
    (should-not (my/ox-gfm-link--url-only-p "https://atlassian.net/browse/A-1"))))

(ert-deftest my/ox-gfm-link-test/url-only-p-match ()
  (let ((my/ox-gfm-link-url-only-prefixes (list my/ox-gfm-link-test--prefix)))
    (should (my/ox-gfm-link--url-only-p "https://atlassian.net/browse/A-1"))))

(ert-deftest my/ox-gfm-link-test/url-only-p-no-match ()
  (let ((my/ox-gfm-link-url-only-prefixes (list my/ox-gfm-link-test--prefix)))
    (should-not (my/ox-gfm-link--url-only-p "https://github.com/foo/bar"))))

;;; GFM export

(ert-deftest my/ox-gfm-link-test/prefix-with-description ()
  (should (string=
           "https://atlassian.net/browse/TASK-1111"
           (my/ox-gfm-link-test--export
            "[[https://atlassian.net/browse/TASK-1111][TASK-1111 title - atlassian.net]]"))))

(ert-deftest my/ox-gfm-link-test/prefix-without-description ()
  (should (string=
           "https://atlassian.net/wiki/spaces/X"
           (my/ox-gfm-link-test--export
            "[[https://atlassian.net/wiki/spaces/X]]"))))

(ert-deftest my/ox-gfm-link-test/other-with-description-kept ()
  (should (string=
           "[foo/bar](https://github.com/foo/bar)"
           (my/ox-gfm-link-test--export "[[https://github.com/foo/bar][foo/bar]]"))))

(ert-deftest my/ox-gfm-link-test/description-equals-url-kept ()
  (should (string=
           "[https://github.com/foo/bar](https://github.com/foo/bar)"
           (my/ox-gfm-link-test--export
            "[[https://github.com/foo/bar][https://github.com/foo/bar]]"))))

(ert-deftest my/ox-gfm-link-test/bracket-link-without-description ()
  (should (string=
           "https://github.com/chongdashu/unreal-mcp"
           (my/ox-gfm-link-test--export "[[https://github.com/chongdashu/unreal-mcp]]"))))

(ert-deftest my/ox-gfm-link-test/plain-url ()
  (should (string=
           "see https://github.com/foo/bar now"
           (my/ox-gfm-link-test--export "see https://github.com/foo/bar now"))))

(ert-deftest my/ox-gfm-link-test/http-url ()
  (should (string=
           "http://example.com/a"
           (my/ox-gfm-link-test--export "[[http://example.com/a]]"))))

(ert-deftest my/ox-gfm-link-test/link-abbrev ()
  (should (string=
           "https://github.com/foo/bar"
           (my/ox-gfm-link-test--export
            "#+LINK: gh https://github.com/%s\n[[gh:foo/bar]]"))))

(ert-deftest my/ox-gfm-link-test/image-kept ()
  (should (string=
           "![img](https://atlassian.net/a.png)"
           (my/ox-gfm-link-test--export "[[https://atlassian.net/a.png]]"))))

(ert-deftest my/ox-gfm-link-test/file-link-kept ()
  (should (string-prefix-p
           "<file:"
           (my/ox-gfm-link-test--export "[[file:/tmp/a.txt]]"))))

;;; Org-roam id links

(ert-deftest my/ox-gfm-link-test/id-link-description-only ()
  (should (string=
           "see ABC now"
           (my/ox-gfm-link-test--export
            "see [[id:0ae58d5e-3668-4dac-a989-8cf051ab574c][ABC]] now"))))

(ert-deftest my/ox-gfm-link-test/id-links-in-headline ()
  (should (string=
           "# ABC - abc"
           (my/ox-gfm-link-test--export
            "* [[id:0ae58d5e-3668-4dac-a989-8cf051ab574c][ABC]] - [[id:5cd90f26-1af9-4d32-953c-56b2107ffbce][abc]]"))))

;;; Other backends untouched

(ert-deftest my/ox-gfm-link-test/md-backend-untouched ()
  (should (string=
           "<https://github.com/foo/bar>"
           (my/ox-gfm-link-test--export "[[https://github.com/foo/bar]]" 'md)))
  (should (string=
           "[TASK-1111](https://atlassian.net/browse/TASK-1111)"
           (my/ox-gfm-link-test--export
            "[[https://atlassian.net/browse/TASK-1111][TASK-1111]]" 'md))))

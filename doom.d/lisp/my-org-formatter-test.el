;;; my-org-formatter-test.el --- Tests for my-org-formatter  -*- lexical-binding: t; -*-

(require 'ert)
(require 'org)
(require 'my-org-formatter)

(defun my/org-formatter-test--run (input expected)
  "Run `my/org-formatter-enforce' on INPUT, compare with EXPECTED."
  (with-temp-buffer
    (org-mode)
    (insert input)
    (my/org-formatter-enforce)
    (should (string= expected (buffer-string)))))

;;; Before heading

(ert-deftest my/org-formatter-test/no-blank-before ()
  (my/org-formatter-test--run
   "text\n* H1\n"
   "text\n\n* H1\n"))

(ert-deftest my/org-formatter-test/multiple-blanks-before ()
  (my/org-formatter-test--run
   "text\n\n\n\n* H1\n"
   "text\n\n* H1\n"))

(ert-deftest my/org-formatter-test/first-heading-at-bob ()
  (my/org-formatter-test--run
   "* H1\n\nbody\n"
   "* H1\n\nbody\n"))

;;; After heading

(ert-deftest my/org-formatter-test/no-blank-after ()
  (my/org-formatter-test--run
   "* H1\nbody\n"
   "* H1\n\nbody\n"))

(ert-deftest my/org-formatter-test/multiple-blanks-after ()
  (my/org-formatter-test--run
   "* H1\n\n\n\nbody\n"
   "* H1\n\nbody\n"))

(ert-deftest my/org-formatter-test/heading-at-eob ()
  (my/org-formatter-test--run
   "text\n\n* H1\n"
   "text\n\n* H1\n"))

;;; Already correct

(ert-deftest my/org-formatter-test/already-correct ()
  (my/org-formatter-test--run
   "text\n\n* H1\n\nbody\n"
   "text\n\n* H1\n\nbody\n"))

;;; Consecutive headings

(ert-deftest my/org-formatter-test/consecutive-headings ()
  (my/org-formatter-test--run
   "* H1\n* H2\n"
   "* H1\n\n* H2\n"))

(ert-deftest my/org-formatter-test/consecutive-headings-multiple-blanks ()
  (my/org-formatter-test--run
   "* H1\n\n\n* H2\n"
   "* H1\n\n* H2\n"))

;;; Planning and drawers

(ert-deftest my/org-formatter-test/with-planning ()
  (my/org-formatter-test--run
   "* TODO H1\nSCHEDULED: <2026-05-13>\nbody\n"
   "* TODO H1\nSCHEDULED: <2026-05-13>\n\nbody\n"))

(ert-deftest my/org-formatter-test/with-drawer ()
  (my/org-formatter-test--run
   "* H1\n:PROPERTIES:\n:ID: abc\n:END:\nbody\n"
   "* H1\n:PROPERTIES:\n:ID: abc\n:END:\n\nbody\n"))

(ert-deftest my/org-formatter-test/with-planning-and-drawer ()
  (my/org-formatter-test--run
   "* TODO H1\nSCHEDULED: <2026-05-13>\n:PROPERTIES:\n:ID: abc\n:END:\nbody\n"
   "* TODO H1\nSCHEDULED: <2026-05-13>\n:PROPERTIES:\n:ID: abc\n:END:\n\nbody\n"))

;;; Nested headings

(ert-deftest my/org-formatter-test/nested-headings ()
  (my/org-formatter-test--run
   "* H1\nbody1\n** H2\nbody2\n"
   "* H1\n\nbody1\n\n** H2\n\nbody2\n"))

;;; org-roam title

(ert-deftest my/org-formatter-test/content-with-title ()
  (my/org-formatter-test--run
   "#+title: My Note\n* H1\n"
   "#+title: My Note\n\n* H1\n"))

(provide 'my-org-formatter-test)
;;; my-org-formatter-test.el ends here

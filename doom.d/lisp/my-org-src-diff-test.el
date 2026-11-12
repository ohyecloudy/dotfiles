;;; my-org-src-diff-test.el --- Tests for my-org-src-diff  -*- lexical-binding: t; -*-

(require 'ert)
(require 'my-org-src-diff)

(defconst my/org-src-diff-test--doc
  "* h\n  #+begin_src diff\n    -removed\n    +added\n\n     context\n  #+end_src\n"
  "Org text with a diff block indented the way `org-adapt-indentation' leaves it.")

(defun my/org-src-diff-test--faces-at (text)
  "Fontify `my/org-src-diff-test--doc' and return the face list at TEXT."
  (with-temp-buffer
    (insert my/org-src-diff-test--doc)
    (let ((org-src-fontify-natively t))
      (org-mode)
      (font-lock-ensure))
    (goto-char (point-min))
    (search-forward "#+begin_src diff\n")
    (search-forward text)
    (let ((face (get-text-property (match-beginning 0) 'face)))
      (if (listp face) face (list face)))))

;;; Indentation helpers

(ert-deftest my/org-src-diff-test/indentation-ignores-blank-lines ()
  (with-temp-buffer
    (insert "    -a\n\n      +b\n")
    (should (= 4 (my/org-src-diff--indentation (point-min) (point-max))))))

(ert-deftest my/org-src-diff-test/indentation-zero-when-flush ()
  (with-temp-buffer
    (insert "-a\n  +b\n")
    (should (= 0 (my/org-src-diff--indentation (point-min) (point-max))))))

(ert-deftest my/org-src-diff-test/line-starts-keep-short-blank-lines ()
  (with-temp-buffer
    (insert "    -a\n  \n    +b")
    (should (equal '(5 10 15)
                   (my/org-src-diff--line-starts (point-min) (point-max) 4)))))

;;; Fontification

(ert-deftest my/org-src-diff-test/removed-line-colored ()
  (should (memq 'diff-removed (my/org-src-diff-test--faces-at "removed"))))

(ert-deftest my/org-src-diff-test/added-line-colored ()
  (should (memq 'diff-added (my/org-src-diff-test--faces-at "added"))))

(ert-deftest my/org-src-diff-test/org-block-face-kept ()
  (should (memq 'org-block (my/org-src-diff-test--faces-at "added"))))

(provide 'my-org-src-diff-test)
;;; my-org-src-diff-test.el ends here

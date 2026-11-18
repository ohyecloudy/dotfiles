;;; my-org-agenda-files-test.el --- Tests for my-org-agenda-files  -*- lexical-binding: t; -*-

(require 'ert)
(require 'org)
(require 'my-org-agenda-files)

(defmacro my/org-agenda-files-test--with-dir (var files &rest body)
  "Bind VAR to a temp directory containing empty FILES, run BODY, then delete it."
  (declare (indent 2))
  `(let ((,var (file-name-as-directory (make-temp-file "agenda-files" t))))
     (unwind-protect
         (progn
           (dolist (f ,files)
             (write-region "" nil (expand-file-name f ,var)))
           ,@body)
       (delete-directory ,var t))))

(defun my/org-agenda-files-test--time (year month day)
  "Return local noon of YEAR-MONTH-DAY."
  (encode-time (list 0 0 12 day month year nil -1 nil)))

(defun my/org-agenda-files-test--names (files)
  "Return sorted base names of FILES."
  (sort (mapcar #'file-name-nondirectory files) #'string<))

;;; Daily

(ert-deftest my/org-agenda-files-test/daily-range-boundaries ()
  (my/org-agenda-files-test--with-dir dir
      '("2026-11-10.org" "2026-11-11.org" "2026-11-18.org"
        "2026-11-25.org" "2026-11-26.org")
    (should (equal (my/org-agenda-files-test--names
                    (my/org-agenda-files-daily
                     dir (my/org-agenda-files-test--time 2026 11 18)))
                   '("2026-11-11.org" "2026-11-18.org" "2026-11-25.org")))))

(ert-deftest my/org-agenda-files-test/daily-missing-dir ()
  (should (null (my/org-agenda-files-daily
                 "/nonexistent-agenda-dir/"
                 (my/org-agenda-files-test--time 2026 11 18)))))

;;; Worklog

(ert-deftest my/org-agenda-files-test/worklog-adjacent-weeks ()
  ;; 2026-11-18 is ISO 2026-W47.
  (my/org-agenda-files-test--with-dir dir
      '("1-worklog_2026_w45_a.org" "2-worklog_2026_w46_a.org"
        "3-worklog_2026_w47_a.org" "4-worklog_2026_w48_a.org"
        "5-worklog_2026_w49_a.org")
    (should (equal (my/org-agenda-files-test--names
                    (my/org-agenda-files-worklog
                     dir (my/org-agenda-files-test--time 2026 11 18)))
                   '("2-worklog_2026_w46_a.org" "3-worklog_2026_w47_a.org"
                     "4-worklog_2026_w48_a.org")))))

(ert-deftest my/org-agenda-files-test/worklog-iso-year-boundary ()
  ;; 2026-12-31 is ISO 2026-W53; next week is ISO 2027-W01.
  (my/org-agenda-files-test--with-dir dir
      '("1-worklog_2026_w52_a.org" "2-worklog_2026_w53_a.org"
        "3-worklog_2027_w01_a.org" "4-worklog_2026_w01_a.org")
    (should (equal (my/org-agenda-files-test--names
                    (my/org-agenda-files-worklog
                     dir (my/org-agenda-files-test--time 2026 12 31)))
                   '("1-worklog_2026_w52_a.org" "2-worklog_2026_w53_a.org"
                     "3-worklog_2027_w01_a.org")))))

(ert-deftest my/org-agenda-files-test/worklog-ignores-other-files ()
  (my/org-agenda-files-test--with-dir dir
      '("note_2026_w47_a.org" "worklog_2026_w47_a.txt" "worklog_2026_w47.org")
    (should (null (my/org-agenda-files-worklog
                   dir (my/org-agenda-files-test--time 2026 11 18))))))

(ert-deftest my/org-agenda-files-test/worklog-missing-dir ()
  (should (null (my/org-agenda-files-worklog
                 "/nonexistent-agenda-dir/"
                 (my/org-agenda-files-test--time 2026 11 18)))))

;;; Auto agenda file tag

(defun my/org-agenda-files-test--update (input)
  "Run `my/org-agenda-update-filetag' on INPUT; return (TEXT . MODIFIED)."
  (with-temp-buffer
    (let ((org-todo-keywords '((sequence "TODO" "STARTED" "|" "DONE"))))
      (org-mode)
      (insert input)
      (set-buffer-modified-p nil)
      (my/org-agenda-update-filetag)
      (cons (buffer-string) (buffer-modified-p)))))

(defun my/org-agenda-files-test--run (input expected)
  "Assert `my/org-agenda-update-filetag' turns INPUT into EXPECTED."
  (should (string= expected (car (my/org-agenda-files-test--update input)))))

(ert-deftest my/org-agenda-files-test/tag-insert-below-last-modified ()
  (my/org-agenda-files-test--run
   ":PROPERTIES:\n:ID: x\n:END:\n#+title: T\n#+last_modified: [2026-11-18]\n\n* TODO a\n"
   ":PROPERTIES:\n:ID: x\n:END:\n#+title: T\n#+last_modified: [2026-11-18]\n#+filetags: :agenda:\n\n* TODO a\n"))

(ert-deftest my/org-agenda-files-test/tag-insert-below-title ()
  (my/org-agenda-files-test--run
   "#+title: T\n\n* TODO a\n"
   "#+title: T\n#+filetags: :agenda:\n\n* TODO a\n"))

(ert-deftest my/org-agenda-files-test/tag-insert-below-drawer ()
  (my/org-agenda-files-test--run
   ":PROPERTIES:\n:ID: x\n:END:\n* TODO a\n"
   ":PROPERTIES:\n:ID: x\n:END:\n#+filetags: :agenda:\n* TODO a\n"))

(ert-deftest my/org-agenda-files-test/tag-insert-at-bob ()
  (my/org-agenda-files-test--run
   "* TODO a\n"
   "#+filetags: :agenda:\n* TODO a\n"))

(ert-deftest my/org-agenda-files-test/tag-append-keeps-other-tags ()
  (my/org-agenda-files-test--run
   "#+title: T\n#+filetags: :ue:csharp:\n\n* STARTED a\n"
   "#+title: T\n#+filetags: :ue:csharp:agenda:\n\n* STARTED a\n"))

(ert-deftest my/org-agenda-files-test/tag-remove-keeps-other-tags ()
  (my/org-agenda-files-test--run
   "#+title: T\n#+filetags: :ue:agenda:csharp:\n\n* DONE a\n"
   "#+title: T\n#+filetags: :ue:csharp:\n\n* DONE a\n"))

(ert-deftest my/org-agenda-files-test/tag-remove-deletes-empty-line ()
  (my/org-agenda-files-test--run
   "#+title: T\n#+filetags: :agenda:\n\n* a\n"
   "#+title: T\n\n* a\n"))

(ert-deftest my/org-agenda-files-test/tag-ignores-checkbox-and-plain-text ()
  (my/org-agenda-files-test--run
   "#+title: T\n\n* a\n- [ ] TODO later\nTODO here\n* TODOS\n"
   "#+title: T\n\n* a\n- [ ] TODO later\nTODO here\n* TODOS\n"))

(ert-deftest my/org-agenda-files-test/tag-nested-todo-heading ()
  (my/org-agenda-files-test--run
   "#+title: T\n\n* a\n** TODO b\n"
   "#+title: T\n#+filetags: :agenda:\n\n* a\n** TODO b\n"))

(ert-deftest my/org-agenda-files-test/tag-ignores-narrowing ()
  (with-temp-buffer
    (let ((org-todo-keywords '((sequence "TODO" "|" "DONE"))))
      (org-mode)
      (insert "#+title: T\n\n* a\n* TODO b\n")
      (goto-char (point-min))
      (re-search-forward "^\\* a")
      (org-narrow-to-subtree)
      (my/org-agenda-update-filetag)
      (widen)
      (should (string= "#+title: T\n#+filetags: :agenda:\n\n* a\n* TODO b\n"
                       (buffer-string))))))

(ert-deftest my/org-agenda-files-test/tag-unchanged-keeps-unmodified ()
  (should-not (cdr (my/org-agenda-files-test--update
                    "#+title: T\n#+filetags: :agenda:\n\n* TODO a\n")))
  (should-not (cdr (my/org-agenda-files-test--update
                    "#+title: T\n\n* DONE a\n"))))

(provide 'my-org-agenda-files-test)
;;; my-org-agenda-files-test.el ends here

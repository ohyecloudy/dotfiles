;;; my-org-agenda-files-test.el --- Tests for my-org-agenda-files  -*- lexical-binding: t; -*-

(require 'ert)
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

(provide 'my-org-agenda-files-test)
;;; my-org-agenda-files-test.el ends here

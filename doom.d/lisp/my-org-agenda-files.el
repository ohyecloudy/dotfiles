;;; my-org-agenda-files.el --- Select org-roam files for org-agenda  -*- lexical-binding: t; -*-

;;; Commentary:
;; Keeps `org-agenda-files' small so agenda builds stay fast on a large
;; org-roam directory: only date-relevant periodic notes are picked.

;;; Code:

(defconst my/org-agenda-files-daily-range 7
  "Number of days before and after today whose daily notes are included.")

(defconst my/org-agenda-files-worklog-week-offsets '(-1 0 1)
  "Week offsets from the current week whose worklog files are included.")

(defun my/org-agenda-files-daily (dir now)
  "Return daily note files in DIR dated within the daily range around NOW.
File names follow the YYYY-MM-DD.org pattern of org-roam dailies."
  (let ((files '()))
    (dotimes (i (1+ (* 2 my/org-agenda-files-daily-range)))
      (let* ((offset (- i my/org-agenda-files-daily-range))
             (date (time-add now (days-to-time offset)))
             (filename (format-time-string "%Y-%m-%d.org" date))
             (filepath (expand-file-name filename dir)))
        (when (file-exists-p filepath)
          (push filepath files))))
    files))

(defun my/org-agenda-files--iso-week (time)
  "Return (ISO-YEAR . ISO-WEEK) of TIME."
  (cons (string-to-number (format-time-string "%G" time))
        (string-to-number (format-time-string "%V" time))))

(defun my/org-agenda-files-worklog (dir now)
  "Return worklog files in DIR for the weeks around NOW.
File names contain worklog_YYYY_wWW_, where YYYY and WW are the ISO year
and week."
  (let ((target-weeks
         (mapcar (lambda (offset)
                   (my/org-agenda-files--iso-week
                    (time-add now (days-to-time (* offset 7)))))
                 my/org-agenda-files-worklog-week-offsets))
        (files '()))
    (when (file-directory-p dir)
      (dolist (f (directory-files dir t "\\.org$"))
        (let ((name (file-name-nondirectory f)))
          (when (string-match "worklog_\\([0-9]\\{4\\}\\)_w\\([0-9]\\{1,2\\}\\)_" name)
            (let ((year (string-to-number (match-string 1 name)))
                  (week (string-to-number (match-string 2 name))))
              (when (member (cons year week) target-weeks)
                (push f files)))))))
    files))

(provide 'my-org-agenda-files)
;;; my-org-agenda-files.el ends here

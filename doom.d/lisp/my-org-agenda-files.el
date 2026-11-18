;;; my-org-agenda-files.el --- Select org-roam files for org-agenda  -*- lexical-binding: t; -*-

;;; Commentary:
;; Keeps `org-agenda-files' small so agenda builds stay fast on a large
;; org-roam directory: only date-relevant periodic notes and files tagged
;; `agenda' (kept in sync with their open TODO headings) are picked.

;;; Code:

;; Only `org-macs' (for `org-with-wide-buffer'): this file is required at
;; startup, and loading all of org there would slow Emacs startup.
(require 'org-macs)

(defvar org-not-done-keywords)
(defvar org-outline-regexp-bol)

(defconst my/org-agenda-tag "agenda"
  "File tag marking org files that hold open TODO headings.")

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

(defconst my/org-agenda-files-roam-dirs '("project/" "weekly/" "monthly/")
  "Subdirectories of the org-roam directory always included as a whole.")

(defun my/org-agenda-files-build (roam-dir extra-dirs now tagged-files)
  "Return the agenda file list for ROAM-DIR at time NOW.
Combines `my/org-agenda-files-roam-dirs', EXTRA-DIRS, date-relevant daily
and worklog files, and TAGGED-FILES (files carrying the `agenda' tag)."
  (delete-dups
   (append (mapcar (lambda (dir) (expand-file-name dir roam-dir))
                   my/org-agenda-files-roam-dirs)
           extra-dirs
           (my/org-agenda-files-daily (expand-file-name "daily/" roam-dir) now)
           (my/org-agenda-files-worklog (expand-file-name "work/" roam-dir) now)
           tagged-files)))

;;; Auto `agenda' file tag

(defun my/org-agenda-has-open-todo-p ()
  "Return non-nil if the buffer has a heading with a not-done TODO keyword.
Checkbox items are not counted."
  (when org-not-done-keywords
    (org-with-wide-buffer
     (goto-char (point-min))
     (let ((case-fold-search nil))
       (re-search-forward
        (format "^\\*+[ \t]+%s\\(?:[ \t]\\|$\\)" (regexp-opt org-not-done-keywords t))
        nil t)))))

(defun my/org-agenda--header-end ()
  "Return the position of the first heading, or `point-max' if none."
  (save-excursion
    (goto-char (point-min))
    (if (re-search-forward org-outline-regexp-bol nil t)
        (line-beginning-position)
      (point-max))))

(defun my/org-agenda--filetags-insert-pos (header-end)
  "Return where a new `#+filetags:' line goes, before HEADER-END.
Below `#+title:'/`#+last_modified:', else below a leading property
drawer, else at the beginning of the buffer."
  (save-excursion
    (goto-char (point-min))
    (let ((case-fold-search t)
          (pos nil))
      (while (re-search-forward "^#\\+\\(?:title\\|last_modified\\):.*$" header-end t)
        (setq pos (min (1+ (point)) header-end)))
      (or pos
          (progn
            (goto-char (point-min))
            (when (and (looking-at-p "[ \t]*:PROPERTIES:")
                       (re-search-forward "^[ \t]*:END:[ \t]*$" header-end t))
              (min (1+ (point)) header-end)))
          (point-min)))))

(defun my/org-agenda--format-filetags (tags)
  "Return the `#+filetags:' line for TAGS, without a trailing newline."
  (concat "#+filetags: :" (mapconcat #'identity tags ":") ":"))

(defun my/org-agenda-update-filetag ()
  "Add or remove the `agenda' file tag to match the buffer's open TODOs.
Leaves the buffer untouched when the tag is already in the right state."
  (org-with-wide-buffer
   (let* ((want (my/org-agenda-has-open-todo-p))
          (header-end (my/org-agenda--header-end))
          (case-fold-search t))
     (goto-char (point-min))
     (if (re-search-forward "^#\\+filetags:[ \t]*\\(.*\\)$" header-end t)
         ;; Save the line bounds first: `split-string' clobbers the match data.
         (let* ((beg (match-beginning 0))
                (end (match-end 0))
                (tags (split-string (match-string 1) "[: \t]+" t))
                (has (member my/org-agenda-tag tags))
                (new-tags (cond ((and want (not has))
                                 (append tags (list my/org-agenda-tag)))
                                ((and has (not want))
                                 (remove my/org-agenda-tag tags)))))
           (when (or (and want (not has)) (and has (not want)))
             (delete-region beg (if new-tags end (min (1+ end) (point-max))))
             (when new-tags
               (goto-char beg)
               (insert (my/org-agenda--format-filetags new-tags)))))
       (when want
         (goto-char (my/org-agenda--filetags-insert-pos header-end))
         (unless (bolp) (insert "\n"))
         (insert (my/org-agenda--format-filetags (list my/org-agenda-tag)) "\n"))))))

(provide 'my-org-agenda-files)
;;; my-org-agenda-files.el ends here

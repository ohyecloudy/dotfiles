;;; my-org-formatter.el --- Org document formatter  -*- lexical-binding: t; -*-

(require 'org)

(defun my/org-formatter--count-blank-lines-above ()
  "Return the number of consecutive blank lines above point."
  (save-excursion
    (let ((count 0))
      (forward-line -1)
      (while (and (not (bobp))
                  (looking-at-p "^[[:space:]]*$"))
        (setq count (1+ count))
        (forward-line -1))
      count)))

(defun my/org-formatter--delete-blank-lines-above ()
  "Delete all consecutive blank lines above point."
  (save-excursion
    (let ((heading-pos (point)))
      (forward-line -1)
      (while (and (not (bobp))
                  (looking-at-p "^[[:space:]]*$"))
        (forward-line -1))
      (unless (looking-at-p "^[[:space:]]*$")
        (forward-line 1))
      (when (< (point) heading-pos)
        (delete-region (point) heading-pos)))))

(defun my/org-formatter--ensure-blank-before ()
  "Ensure exactly one blank line before the heading at point.
Uses `org-with-wide-buffer' to see past `org-map-entries' narrowing."
  (org-with-wide-buffer
   (when (not (bobp))
     (let ((n (my/org-formatter--count-blank-lines-above)))
       (cond
        ((= n 1) nil)
        ((= n 0) (insert "\n"))
        (t
         (my/org-formatter--delete-blank-lines-above)
         (insert "\n")))))))

(defun my/org-formatter--skip-meta ()
  "Move past planning lines and drawers after a heading.
Point should be at the heading.  Moves to the first line after
all meta, returning that position."
  (forward-line 1)
  (while (and (< (point) (point-max))
              (org-at-planning-p))
    (forward-line 1))
  (while (and (< (point) (point-max))
              (looking-at org-drawer-regexp))
    (re-search-forward "^[[:space:]]*:END:.*\n?" (point-max) t)
    (goto-char (match-end 0)))
  (point))

(defun my/org-formatter--count-blank-lines-below ()
  "Return the number of consecutive blank lines at and below point."
  (save-excursion
    (let ((count 0))
      (while (and (< (point) (point-max))
                  (looking-at-p "^[[:space:]]*$"))
        (setq count (1+ count))
        (forward-line 1))
      count)))

(defun my/org-formatter--delete-blank-lines-below ()
  "Delete all consecutive blank lines at and below point."
  (let ((start (point)))
    (while (and (< (point) (point-max))
                (looking-at-p "^[[:space:]]*$"))
      (forward-line 1))
    (when (> (point) start)
      (delete-region start (point)))))

(defun my/org-formatter--ensure-blank-after (end)
  "Ensure exactly one blank line after heading meta, before END.
Point should already be past planning/drawers."
  (when (and (< (point) (point-max))
             (not (= (point) end))
             (not (org-at-heading-p)))
    (let ((n (my/org-formatter--count-blank-lines-below)))
      (cond
       ((= n 1) nil)
       ((= n 0) (insert "\n"))
       (t
        (my/org-formatter--delete-blank-lines-below)
        (insert "\n"))))))

;;;###autoload
(defun my/org-formatter-enforce ()
  "Ensure exactly one blank line before and after each heading."
  (interactive)
  (org-map-entries
   (lambda ()
     (my/org-formatter--ensure-blank-before)
     (unless (org-at-heading-p)
       (outline-next-heading))
     (let ((end (org-entry-end-position)))
       (save-excursion
         (my/org-formatter--skip-meta)
         (my/org-formatter--ensure-blank-after end))))
   t nil))

(provide 'my-org-formatter)
;;; my-org-formatter.el ends here

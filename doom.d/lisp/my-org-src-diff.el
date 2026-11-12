;;; my-org-src-diff.el --- Native fontification for indented diff blocks  -*- lexical-binding: t; -*-

;; Keeps diff src blocks colored when Org indents block content.
;; With `org-src-preserve-indentation' nil, lines reach `diff-mode' as
;; "  -foo", so its line-anchored "^-"/"^+" keywords never match.  This
;; refontifies such blocks from a de-indented copy and maps faces back.

(require 'org)
(require 'org-src)
(require 'diff-mode)

(defconst my/org-src-diff--buffer-name " *my-org-src-diff-fontification*"
  "Scratch buffer used to fontify de-indented diff text.")

(defconst my/org-src-diff--copied-props '(face font-lock-face)
  "Text properties copied from the scratch buffer to the Org buffer.")

(defun my/org-src-diff--indentation (start end)
  "Return the smallest leading space count of non-blank lines in START..END."
  (save-excursion
    (goto-char start)
    (let (min-indent)
      (while (< (point) end)
        (unless (looking-at-p "[ \t]*$")
          (let ((n (skip-chars-forward " " (line-end-position))))
            (setq min-indent (if min-indent (min min-indent n) n))))
        (forward-line 1))
      (or min-indent 0))))

(defun my/org-src-diff--line-starts (start end indent)
  "Return text start positions of lines in START..END after removing INDENT.
Blank lines shorter than INDENT lose only the spaces they have."
  (save-excursion
    (goto-char start)
    (let (starts)
      (while (< (point) end)
        (let ((bol (point)))
          (skip-chars-forward " " (min (+ bol indent) (line-end-position)))
          (push (point) starts))
        (forward-line 1))
      (nreverse starts))))

(defun my/org-src-diff--copy-props (from to target-pos target-end target-buffer)
  "Copy props in FROM..TO of current buffer to TARGET-BUFFER at TARGET-POS.
Copying stops at TARGET-END."
  (let ((pos from))
    (while (< pos to)
      (let* ((next (next-property-change pos nil to))
             (beg (+ target-pos (- pos from)))
             (fin (min target-end (+ target-pos (- next from)))))
        (when (< beg fin)
          (dolist (prop my/org-src-diff--copied-props)
            (let ((value (get-text-property pos prop)))
              (when value
                (put-text-property beg fin prop value target-buffer)))))
        (setq pos next)))))

(defun my/org-src-diff-fontify-indented (lang start end)
  "Refontify an indented diff block between START and END.
Meant as :after advice on `org-src-font-lock-fontify-block'; LANG other
than \"diff\" or blocks without common indentation are left untouched."
  (when (string= lang "diff")
    (let ((indent (my/org-src-diff--indentation start end)))
      (when (> indent 0)
        (let* ((org-buffer (current-buffer))
               (starts (my/org-src-diff--line-starts start end indent))
               (lines (mapcar (lambda (s)
                                (buffer-substring-no-properties
                                 s (save-excursion
                                     (goto-char s)
                                     (min end (line-end-position)))))
                              starts)))
          (with-silent-modifications
            (remove-text-properties start end '(face nil font-lock-face nil))
            (with-current-buffer (get-buffer-create my/org-src-diff--buffer-name)
              (erase-buffer)
              (insert (mapconcat #'identity lines "\n"))
              (unless (eq major-mode 'diff-mode)
                (delay-mode-hooks (diff-mode)))
              (font-lock-ensure)
              (goto-char (point-min))
              (dolist (target starts)
                ;; Include the newline so :extend faces still span the line.
                (let ((bol (point))
                      (next-bol (min (point-max) (1+ (line-end-position)))))
                  (my/org-src-diff--copy-props bol next-bol target end org-buffer)
                  (goto-char next-bol))))
            (let ((src-face (nth 1 (assoc-string lang org-src-block-faces t))))
              (when (or (facep src-face) (listp src-face))
                (font-lock-append-text-property start end 'face src-face))
              (font-lock-append-text-property start end 'face 'org-block))))))))

(advice-add 'org-src-font-lock-fontify-block :after #'my/org-src-diff-fontify-indented)

(provide 'my-org-src-diff)
;;; my-org-src-diff.el ends here

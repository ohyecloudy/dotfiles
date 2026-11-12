;;; org-format.el --- Normalize skill-generated .org files to the state emacs leaves after open+save -*- lexical-binding: t -*-

;; Usage: emacs -Q --batch -l org-format.el <file.org>...

(require 'org)

(defconst org-format-block-content-offset 2
  "Columns block content sits right of its #+begin line.
Matches the default `org-edit-src-content-indentation', so C-c ' round trips without diff.")

(defconst org-format-block-begin-re "^[ \t]*#\\+begin_\\(src\\|example\\)\\b"
  "Blocks whose content indentation and surrounding blank lines get normalized.")

(defun org-format--blank-line-p ()
  (looking-at-p "[ \t]*$"))

(defun org-format--affiliated-line-p ()
  "Keyword lines that belong to the block below them (#+name, #+caption, ...)."
  (looking-at-p "[ \t]*#\\+\\(name\\|caption\\|header\\|plot\\|attr_[a-z0-9_-]+\\):"))

(defun org-format--results-line-p ()
  "Keyword line that belongs to the block above it."
  (looking-at-p "[ \t]*#\\+results\\b"))

(defun org-format--reindent-content (content-beg content-end target)
  "Shift non-blank lines between CONTENT-BEG and CONTENT-END so the least indented one sits at TARGET.
Relative indentation, diff prefixes and whitespace-only lines are kept as is."
  (let ((min-indent nil))
    (save-excursion
      (goto-char content-beg)
      (while (< (point) content-end)
        (unless (org-format--blank-line-p)
          (setq min-indent (min (or min-indent most-positive-fixnum) (current-indentation))))
        (forward-line 1)))
    (when (and min-indent (/= min-indent target))
      (let ((delta (- target min-indent))
            (end (copy-marker content-end)))
        (save-excursion
          (goto-char content-beg)
          (while (< (point) end)
            (unless (org-format--blank-line-p)
              (indent-line-to (+ (current-indentation) delta)))
            (forward-line 1)))
        (set-marker end nil)))))

(defun org-format--ensure-blank-before (begin-line)
  "Put one blank line above BEGIN-LINE, above its affiliated keywords (#+name etc.)."
  (save-excursion
    (goto-char begin-line)
    (let ((case-fold-search t)
          (above-p nil))
      (while (and (= (forward-line -1) 0)
                  (setq above-p t)
                  (org-format--affiliated-line-p))
        (setq above-p nil))
      ;; ABOVE-P: point is on the line right above the block and its keywords.
      (when (and above-p (not (org-format--blank-line-p)))
        (end-of-line)
        (insert "\n")))))

(defun org-format--ensure-blank-after (end-line)
  "Put one blank line below END-LINE unless #+RESULTS or buffer end follows."
  (save-excursion
    (goto-char end-line)
    (when (and (= (forward-line 1) 0)
               (not (eobp))
               (not (org-format--blank-line-p))
               (not (let ((case-fold-search t)) (org-format--results-line-p))))
      (insert "\n"))))

(defun org-format--normalize-blocks ()
  (let ((case-fold-search t)
        (blocks '()))
    (goto-char (point-min))
    (while (re-search-forward org-format-block-begin-re nil t)
      (let* ((begin-line (copy-marker (line-beginning-position)))
             (end-re (format "^[ \t]*#\\+end_%s\\b" (match-string 1))))
        (forward-line 1)
        (if (re-search-forward end-re nil t)
            (push (list begin-line (copy-marker (line-beginning-position))) blocks)
          (message "org-format: unterminated block at line %d, skipped"
                   (line-number-at-pos begin-line))
          (goto-char (point-max)))))
    ;; Last block first, so edits never shift a block not yet processed.
    (dolist (block blocks)
      (let* ((begin-line (nth 0 block))
             (end-line (nth 1 block))
             (target (+ (save-excursion (goto-char begin-line) (current-indentation))
                        org-format-block-content-offset)))
        (org-format--reindent-content (save-excursion (goto-char begin-line) (line-beginning-position 2))
                                      end-line target)
        (org-format--ensure-blank-after end-line)
        (org-format--ensure-blank-before begin-line)
        (set-marker begin-line nil)
        (set-marker end-line nil)))))

(defun org-format-file (file)
  (find-file file)
  (org-mode)
  (setq indent-tabs-mode nil)
  (org-indent-region (point-min) (point-max))
  (org-format--normalize-blocks)
  ;; Fill [/] cookies so skills never count TODO children by hand.
  (org-update-statistics-cookies 'all)
  (set-buffer-file-coding-system (if (memq system-type '(windows-nt ms-dos)) 'utf-8-dos 'utf-8-unix))
  (save-buffer))

(setq make-backup-files nil)
(setq org-adapt-indentation t)
;; Keep block content away from language modes: they would reindent code and drop diff context prefixes.
(setq org-src-tab-acts-natively nil)

(let ((dir default-directory))
  (dolist (f command-line-args-left)
    (org-format-file (expand-file-name f dir))))
(setq command-line-args-left nil)

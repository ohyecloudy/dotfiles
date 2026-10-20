;;; my-rider.el --- Hand the current buffer over to JetBrains Rider  -*- lexical-binding: t; -*-

;;; Commentary:

;; Bridge from Emacs to Rider: open the visited file at point in Rider,
;; inside the nearest .uproject/.slnx/.sln found walking up from the file.
;;
;; Whether the project is already open is left to Rider's launcher, which
;; forwards to the running instance and only opens the file when the
;; project window exists.  Without a project the file opens in LightEdit.

;;; Code:

(require 'seq)
(require 'subr-x)

(defgroup my-rider nil
  "Open files in JetBrains Rider."
  :group 'tools)

(defcustom my/rider-executable nil
  "Path to rider64.exe.  When nil, pick the newest match of `my/rider-install-glob'."
  :type '(choice (const :tag "Auto detect" nil) file)
  :group 'my-rider)

(defcustom my/rider-install-glob
  "C:/Program Files/JetBrains/JetBrains Rider */bin/rider64.exe"
  "Wildcard used to find Rider installs when `my/rider-executable' is nil."
  :type 'string
  :group 'my-rider)

(defconst my/rider-project-extensions '("uproject" "slnx" "sln")
  "Project file extensions, highest priority first.")

(defconst my/rider--install-version-regexp
  "/JetBrains Rider \\([0-9]+\\(?:\\.[0-9]+\\)*\\)/"
  "Extract the version from a Rider install path.")

(defun my/rider--project-in-dir (dir)
  "Return sorted project files in DIR for the highest-priority extension, or nil."
  (when (file-directory-p dir)
    (seq-some (lambda (ext)
                (seq-filter #'file-regular-p
                            (directory-files dir t (concat "\\." (regexp-quote ext) "\\'"))))
              my/rider-project-extensions)))

(defun my/rider-find-project (file)
  "Return project candidates in the nearest ancestor directory of FILE, or nil."
  (when-let* ((root (locate-dominating-file (file-name-directory (expand-file-name file))
                                            #'my/rider--project-in-dir)))
    (my/rider--project-in-dir root)))

(defun my/rider--default-candidate (dir candidates)
  "Return the candidate named after DIR, else the first of CANDIDATES."
  (let ((dir-name (downcase (file-name-nondirectory (directory-file-name dir)))))
    (or (seq-find (lambda (c) (string= (downcase (file-name-base c)) dir-name))
                  candidates)
        (car candidates))))

(defun my/rider--latest-executable (paths)
  "Return the path in PATHS with the highest Rider version, or nil.
Paths without a parsable version are ignored."
  (car (sort (seq-filter (lambda (p) (string-match-p my/rider--install-version-regexp p))
                         paths)
             (lambda (a b)
               (version< (my/rider--install-version b) (my/rider--install-version a))))))

(defun my/rider--install-version (path)
  "Return the version string embedded in install PATH."
  (when (string-match my/rider--install-version-regexp path)
    (match-string 1 path)))

(defun my/rider--native-path (path)
  "Return PATH with Windows separators."
  (subst-char-in-string ?/ ?\\ path))

(defun my/rider--command-args (project file line column)
  "Return rider64.exe arguments opening FILE at LINE/COLUMN in PROJECT.
PROJECT may be nil to open the file alone."
  (append (when project (list (my/rider--native-path project)))
          (list "--line" (number-to-string line)
                "--column" (number-to-string column)
                (my/rider--native-path file))))

(defun my/rider--resolve-executable ()
  "Return a runnable rider64.exe path or signal `user-error'."
  (let ((exe (or my/rider-executable
                 (my/rider--latest-executable (file-expand-wildcards my/rider-install-glob)))))
    (unless (and exe (file-executable-p exe))
      (user-error "Rider not found: set `my/rider-executable' (tried %s)"
                  (or exe my/rider-install-glob)))
    exe))

(defun my/rider--choose-project (candidates)
  "Return one project from CANDIDATES, prompting when there are several."
  (if (cdr candidates)
      (let ((default (my/rider--default-candidate
                      (file-name-directory (car candidates)) candidates)))
        (completing-read "Rider project: " candidates nil t nil nil default))
    (car candidates)))

(defun my/open-in-rider ()
  "Open the visited file at point in Rider within its nearest project."
  (interactive)
  (let ((file (buffer-file-name)))
    (unless file
      (user-error "Buffer is not visiting a file"))
    (let ((exe (my/rider--resolve-executable)))
      (when (buffer-modified-p)
        (save-buffer))
      (let* ((project (my/rider--choose-project (my/rider-find-project file)))
             (line (line-number-at-pos))
             (column (1+ (- (point) (line-beginning-position)))))
        (apply #'call-process exe nil 0 nil
               (my/rider--command-args project file line column))
        (if project
            (message "Opening %s in Rider (%s)"
                     (file-name-nondirectory file) (file-name-nondirectory project))
          (message "No sln/slnx/uproject found, opening file only: %s"
                   (file-name-nondirectory file)))))))

(provide 'my-rider)
;;; my-rider.el ends here

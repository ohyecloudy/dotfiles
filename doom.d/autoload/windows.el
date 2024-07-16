;;; autoload/windows.el -*- lexical-binding: t; -*-
;;; reference os/macos/autoload.el in Doom Emacs

(defun +windows-explorer-with (&optional path)
  (interactive)
  (let* ((path (expand-file-name
                (replace-regexp-in-string
                 "'" "\\'"
                 (or path (if (derived-mode-p 'dired-mode)
                              (dired-get-file-for-visit)
                            (buffer-file-name)))
                 nil t)))
         (path (+windows--remove-trailing-backslash (replace-regexp-in-string "/" "\\\\" path)))
         (select-arg (format "/select,%s" path)))
    (message "Running: explorer.exe %s" select-arg)
    (w32-shell-execute "open" "explorer.exe" select-arg)))

(defun +windows-open-with-cmd (&optional dir)
  (interactive)
  (let* ((dir (expand-file-name
               (replace-regexp-in-string
                "'" "\\'"
                (or dir (if (derived-mode-p 'dired-mode)
                            (dired-get-subdir)
                          default-directory))
                nil t))))
    (message "Running: cmd.exe /K cd /d %s" dir)
    (w32-shell-execute "open" "cmd.exe" (concat "/K cd /d" dir))))

(defun +windows-open-with-bash (&optional dir)
  (interactive)
  (let* ((dir (expand-file-name
               (replace-regexp-in-string
                "'" "\\'"
                (or dir (if (derived-mode-p 'dired-mode)
                            (dired-get-subdir)
                          default-directory))
                nil t))))
    (message "Running: git-bash.exe --cd=%s" dir)
    (w32-shell-execute "open" "git-bash.exe" (format "--cd=%s" dir))))

(defmacro +windows--explorer-with (id &optional dir)
  `(defun ,(intern (format "+windows/%s" id)) ()
     (interactive)
     (+windows-explorer-with ,dir)))

(defmacro +windows--open-with-cmd (id &optional dir)
  `(defun ,(intern (format "+windows/%s" id)) ()
     (interactive)
     (+windows-open-with-cmd ,dir)))

(defmacro +windows--open-with-bash (id &optional dir)
  `(defun ,(intern (format "+windows/%s" id)) ()
     (interactive)
     (+windows-open-with-bash ,dir)))

(defun +windows--remove-trailing-backslash (str)
  (if (string-suffix-p "\\" str)
      (substring str 0 -1)
    str))

;;;###autoload (autoload '+windows/reveal-in-explorer "autoload/windows" nil t)
(+windows--explorer-with reveal-in-explorer (buffer-file-name))

;;;###autoload (autoload '+windows/reveal-project-in-explorer "autoload/windows" nil t)
(+windows--explorer-with reveal-project-in-explorer
                         (or (doom-project-root) default-directory))

;;;###autoload (autoload '+windows/open-in-cmd "autoload/windows" nil t)
(+windows--open-with-cmd open-in-cmd default-directory)

;;;###autoload (autoload '+windows/open-in-bash "autoload/windows" nil t)
(+windows--open-with-bash open-in-bash default-directory)

;;; lisp/my-org-bulk-export-jekyll.el -*- lexical-binding: t; -*-

(defvar my-org-bulk-export-jekyll--log-buffer-name "*My Org Bulk Jekyll Export Log*")

(defun my-org-bulk-export-jekyll ()
  (interactive)
  (let* ((posts-dir (my-org-bulk-export-jekyll-locate-posts-dir))
         (org-files (directory-files-recursively posts-dir "\\.org$"))
         (total (length org-files))
         (counter 0))

    (my-org-bulk-export-jekyll--log "Found %d .org files to export." total)

    (dolist (file org-files)
      (setq counter (1+ counter))
      (my-org-bulk-export-jekyll--log "Exporting %d/%d: %s" counter total file)
      (with-current-buffer (find-file-noselect file)
        (let ((org-export-with-broken-links 'mark))
          (org-export-to-file
              'jekyll
              (concat (file-name-sans-extension file) ".md")))))))

(defun my-org-bulk-export-jekyll-locate-posts-dir ()
  "Locate the _posts directory at the root of the current Git repository, if any."
  (let ((root (vc-git-root default-directory)))
    (when root (concat root "_posts/"))))

(defun my-org-bulk-export-jekyll--log (format-string &rest args)
  "Log a message to the dedicated export log buffer."
  (with-current-buffer (get-buffer-create my-org-bulk-export-jekyll--log-buffer-name)
    (let ((inhibit-read-only t)) ;; allow writing to read-only buffer
      (goto-char (point-max))
      (insert (apply #'format format-string args) "\n"))))

(provide 'my-org-bulk-export-jekyll)

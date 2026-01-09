;;; my-org-utils.el --- Org link utilities  -*- lexical-binding: t; -*-

(require 'org)
(require 'ol)
(require 'seq)

(defun my/org-url-to-abbrev-link (url)
  "Convert URL to an Org abbrev link if a matching abbrev is defined.

Searches `org-link-abbrev-alist-local' and `org-link-abbrev-alist'
for an abbrev whose template matches URL.

Return \"KEY:PATH\" if matched; otherwise nil."
  (when (stringp url)
    (let* ((alist (append org-link-abbrev-alist-local
                          org-link-abbrev-alist))
           (match
            (seq-find
             (lambda (entry)
               (let ((template (cdr entry)))
                 (cond
                  ((and (stringp template) (string-match "%s" template))
                   (string-prefix-p
                    (replace-regexp-in-string "%s" "" template)
                    url))
                  ((stringp template)
                   (string-prefix-p template url)))))
             alist)))
      (when match
        (let* ((key (car match))
               (template (cdr match))
               (prefix (replace-regexp-in-string "%s" "" template)))
          (format "%s:%s" key (substring url (length prefix))))))))

(provide 'my-org-utils)
;;; my-org-utils.el ends here

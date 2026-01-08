;;; my-org-cliplink.el --- Customize org-cliplink to my liking -*- lexical-binding: t; -*-

;; Copyright (C) 2024 Jongbin Oh

;; Author: Jongbin Oh <ohyecloudy@gmail.com>
;; Created: 2024-03-30

(require 'org-cliplink)

(defcustom my/org-cliplink-custom-retrieve-title-hook nil
  "Used when retrieving the title using a method other
than obtaining the title by visiting a web page.
If nil is returned, the web page is visited and the title is obtained.

e.g. A page that obtains the title using the API. jira, confluence."
  :type 'hook
  :group 'my/org-cliplink)

(defcustom my/org-cliplink-host-transform-rules
  '(("^www\\." ""))
  "Alist of (REGEXP REPLACEMENT) rules used to transform host names in org-cliplink."
  :group 'org-cliplink
  :type '(repeat
          (list (regexp :tag "Match regexp")
                (string :tag "Replacement"))))

(defun my/org-cliplink ()
  (interactive)
  (let* ((url (org-cliplink-clipboard-content))
         (title (when my/org-cliplink-custom-retrieve-title-hook
                  (funcall my/org-cliplink-custom-retrieve-title-hook url))))
    (if title
        (insert (funcall #'my/org-cliplink-link-transformer url title))
      (org-cliplink-insert-transformed-title
       url
       #'my/org-cliplink-link-transformer))))

(defun my/org-cliplink-link-transformer (url title)
  (let* ((title (or title (org-cliplink-retrieve-title-synchronously url))))
    ;; If title is still nil after trying to retrieve it synchronously
    (when (null title)
      (error "Failed to retrieve title for URL: %s" url))
    (let* ((parsed-url (url-generic-parse-url url)) ;parse the url
           (host-url (my/org-cliplink--host-transform-rules (url-host parsed-url)))
           (clean-title
            (cond
             ;; if the host is github.com, cleanup the title
             ((string= (url-host parsed-url) "github.com")
              (replace-regexp-in-string "^/" ""
                                        (car (url-path-and-query parsed-url))))
             ;; otherwise keep the original title
             (t (my/org-cliplink--cleansing-site-title title))))
           (title-with-url (format "%s - %s" clean-title host-url))
           (abbrev (my/org-cliplink--url-to-abbrev-link url))
           (final-link (or abbrev url)))
      ;; forward the title to the default org-cliplink transformer
      (org-cliplink-org-mode-link-transformer final-link title-with-url))))

(defun my/org-cliplink--url-to-abbrev-link (url)
  "Convert URL to an Org abbrev link if a matching abbrev is defined.

Searches `org-link-abbrev-alist-local' and `org-link-abbrev-alist'
for an abbrev whose template matches URL.

Supported abbrev forms:

  1. Template with %s
     #+LINK: gl https://gitlab.com/%s
     => \"https://gitlab.com/foo/bar\" → \"gl:foo/bar\"

  2. Prefix-only template
     #+LINK: gh https://github.com/
     => \"https://github.com/emacs-mirror/emacs\"
        → \"gh:emacs-mirror/emacs\"

If a matching abbrev is found, return a string of the form
\"KEY:PATH\".  If no abbrev matches URL, return nil.

URL must be a string."
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
        (format "%s:%s" key (substring url (length prefix)))))))

(defun my/org-cliplink--host-transform-rules (host)
  (let ((result host))
    (dolist (rule my/org-cliplink-host-transform-rules result)
      (setq result (replace-regexp-in-string (car rule) (cadr rule) result t)))
    result))

(defun my/org-cliplink--cleansing-site-title (title)
  (let ((result title)
        (from-to-titles '((" - 위키백과, 우리 모두의 백과사전" "")
                          (" - Wikipedia" "")
                          (" - PUBLY" "")
                          (" - YES24" "")
                          ("알라딘: " "")
                          (" : 클리앙" "")
                          (" - YouTube" "")
                          ("|" "-"))))
    (dolist (elem from-to-titles)
      (let ((from (car elem))
            (to (cadr elem)))
        (if (string-match from result)
            (setq result (string-replace from to result))
          result)))
    result))

(provide 'my-org-cliplink)
;;; my-org-cliplink.el ends here

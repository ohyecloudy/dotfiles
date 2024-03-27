;;; my-org-cliplink.el --- Customize org-cliplink to my liking -*- lexical-binding: t; -*-

;; Copyright (C) 2024 Jongbin Oh

;; Author: Jongbin Oh <ohyecloudy@gmail.com>
;; Created: 2024-03-30

(require 'org-cliplink)

(defun my/org-cliplink ()
  (interactive)
  (org-cliplink-insert-transformed-title
   (org-cliplink-clipboard-content)     ;take the URL from the CLIPBOARD
   #'my/org-cliplink-link-transformer))

(defun my/org-cliplink-link-transformer (url title)
  (let* ((parsed-url (url-generic-parse-url url)) ;parse the url
         (host-url (replace-regexp-in-string "^www\\." "" (url-host parsed-url)))
         (clean-title
          (cond
           ;; if the host is github.com, cleanup the title
           ((string= (url-host parsed-url) "github.com")
            (replace-regexp-in-string "^/" ""
                                      (car (url-path-and-query parsed-url))))
           ;; otherwise keep the original title
           (t (my/org-cliplink--cleansing-site-title title))))
         (title-with-url (format "%s - %s" clean-title host-url)))
    ;; forward the title to the default org-cliplink transformer
    (org-cliplink-org-mode-link-transformer url title-with-url)))

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

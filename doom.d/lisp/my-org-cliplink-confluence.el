;;; my-org-cliplink-confluence.el -*- lexical-binding: t; -*-

;; Copyright (C) 2024 Jongbin Oh

;; Author: Jongbin Oh <ohyecloudy@gmail.com>
;; Created: 2024-04-30

(defcustom my/org-cliplink-confluence-host-api-urls '()
  "Alist of api url about host
Each element has the form (HOST . API-URL)."
  :type '(alist :key-type string :value-type string)
  :group 'my/org-cliplink-confluence)

(defun my/org-cliplink-confluence-title (url)
  (when-let* ((host-api (my/org-cliplink-confluence--find-host url my/org-cliplink-confluence-host-api-urls))
              (host (car host-api))
              (api-base-url (cdr host-api))
              (page-id (my/org-cliplink-confluence--page-id url))
              (api-url (my/org-cliplink-confluence--api-content-get api-base-url page-id))
              (auth-header (my/org-cliplink-confluence--auth host))
              (content (my/org-cliplink-confluence--retrieve-content
                        (my/org-cliplink-confluence--api-content-get api-base-url page-id)
                        auth-header)))
    (format "%s > %s" (plist-get content :space) (plist-get content :title))))

(defun my/org-cliplink-confluence--api-content-get (base-url id)
  (format "%s/rest/api/content/%s" base-url id))

(defun my/org-cliplink-confluence--retrieve-content (url auth-header)
  (let ((filter-fun (lambda (content)
                      (let ((title (plist-get content :title))
                            (space (plist-get (plist-get content :space) :name)))
                        (list :title title :space space)))))
    (my/org-cliplink-confluence--get url
                                     auth-header
                                     filter-fun)))

(defun my/org-cliplink-confluence--get (url authorization-header filter-fun)
  (let ((url-request-extra-headers `(,authorization-header
                                     ("Content-type" . "application/json; charset=utf-8")))
        (json-key-type 'keyword)
        (json-object-type 'plist))
    (with-temp-buffer
      (url-insert-file-contents url)
      (let ((content (json-read)))
        (funcall filter-fun content)))))

(defun my/org-cliplink-confluence--find-host (url host-api-urls)
  (car (seq-filter (lambda (elem)
                     (let ((host (car elem)))
                       (string-match-p (regexp-quote host) url)))
                   host-api-urls)))

(defun my/org-cliplink-confluence--page-id (url)
  (save-match-data
    (if (string-match "\\?pageId=\\([[:digit:]]+\\)" url)
        (match-string 1 url))))

(defun my/org-cliplink-confluence--auth (host)
  (when-let* ((id-password (my/org-cliplink-confluence--id-password host))
              (encoded (base64url-encode-string
                        (format "%s:%s" (car id-password)
                                (cadr id-password)))))
    `("Authorization" . ,(format "Basic %s" encoded))))

(defun my/org-cliplink-confluence--id-password (host)
  (when-let* ((found (auth-source-search
                      :host host
                      :requires '(:secret)))
              (first-found (nth 0 found))
              (id (plist-get first-found :user))
              (password (funcall (plist-get first-found :secret))))
    (list id password)))

(provide 'my-org-cliplink-confluence)
;;; my/org-cliplink-confluence.el ends here

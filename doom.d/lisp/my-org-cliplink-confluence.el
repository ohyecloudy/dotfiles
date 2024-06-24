;;; my-org-cliplink-confluence.el -*- lexical-binding: t; -*-

;; Copyright (C) 2024 Jongbin Oh

;; Author: Jongbin Oh <ohyecloudy@gmail.com>
;; Created: 2024-04-30

(defcustom my/org-cliplink-confluence-hosts '()
  "Property List of confluence host
Ecah element has the form '((:url MY_CONFLUENCE_URL :api-version 6)
(:url MY_CONFLUECNE_URL :api-version :cloud))"
  :type '(plist)
  :group 'my/org-cliplink-confluence)

(defun my/org-cliplink-confluence-title (url)
  (when-let* ((host-info (my/org-cliplink-confluence--find-host url my/org-cliplink-confluence-hosts))
              (api-version (plist-get host-info :api-version))
              (extract-page-id-fun (my/org-cliplink-confluence--extract-page-id-fun api-version))
              (build-content-get-api-fun (my/org-cliplink-confluence--build-content-get-api-fun api-version))
              (page-id (funcall extract-page-id-fun url))
              (api-base-url (plist-get host-info :url))
              (api-url (funcall build-content-get-api-fun api-base-url page-id))
              (auth-header (my/org-cliplink-confluence--auth (my/org-cliplink-confluence--remove-protocol (plist-get host-info :url))))
              (content (my/org-cliplink-confluence--retrieve-content api-url auth-header)))
    (format "%s" (plist-get content :title))))

(defun my/org-cliplink-confluence--find-host (url hosts)
  (car
   (seq-filter (lambda (elem)
                 (let ((host-url (plist-get elem :url)))
                   (string-match-p (regexp-quote host-url) url)))
               hosts)))

(defun my/org-cliplink-confluence--remove-protocol (url)
  (let* ((parsed (url-generic-parse-url url))
         (host (url-host parsed))
         (path (url-filename parsed)))
    (format "%s%s" host path)))

(defun my/org-cliplink-confluence--extract-page-id-fun (api-version)
  (pcase api-version
    (:cloud (lambda (url)
              (save-match-data
                (if (string-match "/pages/\\([[:digit:]]+\\)\\($\\|/\\)" url)
                    (match-string 1 url)))))
    (6 (lambda (url)
         (save-match-data
           (if (string-match "\\?pageId=\\([[:digit:]]+\\)" url)
               (match-string 1 url)))))))

(defun my/org-cliplink-confluence--build-content-get-api-fun (api-version)
  (pcase api-version
    (:cloud (lambda (base-url id)
              (format "%s/api/v2/pages/%s" base-url id)))
    (6 (lambda (base-url id)
         (format "%s/rest/api/content/%s" base-url id)))))

(defun my/org-cliplink-confluence--api-content-get (base-url id)
  )

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

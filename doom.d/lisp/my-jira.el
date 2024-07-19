;;; lisp/my-jira.el -*- lexical-binding: t; -*-

(defcustom my/jira-hosts '()
  "Property List of jira host
Ecah element has the form '((:url MY_JIRA :api-url MY_JIRA_API :api-version :cloud))"
  :type '(plist)
  :group 'my-jira)

(defun my/jira-insert-heading-content ()
  (interactive)
  (let* ((url (my/jira--url-clipboard-or-prompt))
         (content (my/jira--content url))
         (summary (plist-get content :summary))
         (id (plist-get content :id)))
    (org-insert-heading)
    (insert (format "%s %s [/]" id summary))
    (org-update-statistics-cookies nil)
    (org-set-property "URL" url)))

(defun my/jira-title (url)
  (when-let* ((content (my/jira--content url)))
    (format "%s %s" (plist-get content :id) (plist-get content :summary))))

(defun my/jira--content (url)
  (when-let* ((host-info (my/jira--find-host url my/jira-hosts))
              (jira-id (my/jira--issue-id url))
              (api-url (my/jira--build-issue-api-url (plist-get host-info :api-url) jira-id))
              (auth-header (my/jira--auth (my/jira--remove-protocol (plist-get host-info :url))))
              (content (my/jira--retrieve-content api-url auth-header)))
    (plist-put content :id jira-id)))

(defun my/jira--url-clipboard-or-prompt ()
  (let ((clipboard-content (gui-get-selection 'CLIPBOARD)))
    (if (and clipboard-content (string-match-p "^http" clipboard-content))
        clipboard-content
      (read-string "Enter URL: "))))

(defun my/jira--find-host (url hosts)
  (car (seq-filter (lambda (elem)
                     (let ((host-url (plist-get elem :url)))
                       (string-match-p (regexp-quote host-url) url)))
                   hosts)))

(defun my/jira--issue-id (url)
  (save-match-data
    (if (string-match "/browse/\\([^/]+\\)" url)
        (match-string 1 url))))

(defun my/jira--build-issue-api-url (base-url issue-id)
  (let ((base-url (string-remove-suffix "/" base-url)))
    (format "%s/api/2/issue/%s" base-url issue-id)))

(defun my/jira--remove-protocol (url)
  (let* ((parsed (url-generic-parse-url url))
         (host (url-host parsed))
         (path (url-filename parsed)))
    (format "%s%s" host path)))

(defun my/jira--auth (host)
  (when-let* ((id-password (my/jira--id-password host))
              (encoded (base64url-encode-string
                        (format "%s:%s" (car id-password)
                                (cadr id-password)))))
    `("Authorization" . ,(format "Basic %s" encoded))))

(defun my/jira--id-password (host)
  (when-let* ((found (auth-source-search
                      :host host
                      :requires '(:secret)))
              (first-found (nth 0 found))
              (id (plist-get first-found :user))
              (password (funcall (plist-get first-found :secret))))
    (list id password)))

(defun my/jira--retrieve-content (url auth-header)
  (let ((filter-fun (lambda (content)
                      (let* ((fields (plist-get content :fields))
                             (summary (plist-get fields :summary))
                             (description (plist-get fields :description)))
                        (list :summary summary :description description)))))
    (my/jira--get url
                  auth-header
                  filter-fun)))

(defun my/jira--get (url authorization-header filter-fun)
  (let ((url-request-extra-headers `(,authorization-header
                                     ("Content-type" . "application/json; charset=utf-8")))
        (json-key-type 'keyword)
        (json-object-type 'plist))
    (with-temp-buffer
      (url-insert-file-contents url)
      (let ((content (json-read)))
        (funcall filter-fun content)))))

(provide 'my-jira)
;;; my-jira.el ends here

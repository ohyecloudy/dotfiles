;;; lisp/my-jira.el -*- lexical-binding: t; -*-

(defun my/jira-insert-issue-heading (issue-number)
  (interactive "nissue number: ")
  (let* ((issue-id (my/jira--build-issue-id (my/jira--get-secret "project") issue-number))
         (api-url (my/jira--build-issue-api-url (my/jira--get-secret "api-base-url") issue-id))
         (web-url (my/jira--build-issue-web-url (my/jira--get-secret "api-base-url") issue-id))
         (summary (my/jira--issue-summary api-url (my/jira--get-secret "access-key"))))
    (message "%S" summary)
    (org-insert-heading)
    (insert (format "%s %s [/]" issue-id summary))
    (org-update-statistics-cookies nil)
    (org-set-property "Url" web-url)
    )
  )

(defun my/jira--get-secret (key)
  (funcall
   (plist-get
    (nth 0 (auth-source-search
            :host "myjira"
            :user key
            :requires '(:secret)))
    :secret))
  )

(defun my/jira--build-issue-id (project issue-number)
  (format "%s-%d" project issue-number))

(defun my/jira--build-issue-api-url (base-url issue-id)
  (let ((base-url (string-remove-suffix "/" base-url)))
    (format "%s/rest/agile/1.0/issue/%s" base-url issue-id)))

(defun my/jira--build-issue-web-url (base-url issue-id)
  (let ((base-url (string-remove-suffix "/" base-url)))
    (format "%s/browse/%s" base-url issue-id)))

(defun my/jira--issue-summary (url access-key)
  (let ((extract-summary_func (lambda (item)
                                (when (and (listp item)
                                           (plist-member item :summary))
                                  (plist-get item :summary))
                                )))
    (car (my/jira--request url
                           access-key
                           extract-summary_func))))

(defun my/jira--request (url access-key response-func)
  (let ((url-request-extra-headers `(("Authorization" . ,(format "Bearer %s" access-key))
                                     ("Content-type" . "application/json; charset=utf-8")))
        (json-key-type 'keyword)
        (json-object-type 'plist)
        (result '()))
    (with-temp-buffer
      (url-insert-file-contents url)
      (let ((content (json-read)))
        (dolist (c content)
          (push (funcall response-func c) result))
        ))
    (remove nil result)))

(provide 'my-jira)
;;; my-jira.el ends here

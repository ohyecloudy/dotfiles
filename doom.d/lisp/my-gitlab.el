;;; my-gitlab.el --- gitlab helper package

(defun my-gitlab-open-todo ()
  (interactive)
  (when (not (boundp 'gitlab-api-url)) (throw 'gitlab-api-url "not bound"))
  (let ((todos (my-gitlab--request-get (my-gitlab--build-request
                                        gitlab-api-url
                                        "todos"
                                        '(("per_page" 50))))))
    (dolist (elt todos)
      (browse-url (plist-get elt :target_url)))))

(defun my-gitlab-open-issue-error ()
  (interactive)
  (when (not (boundp 'gitlab-team-members)) (throw 'gitlab-team-members "not bound"))
  (switch-to-buffer (make-temp-name "gitlab error issues"))
  (org-mode)
  (my-gitlab--insert-issues-by (list '("labels" "에러") '("assignee_id" "None")))
  (dolist (elt gitlab-team-members)
    (message elt)
    (my-gitlab--insert-issues-by (list '("labels" "에러") (list "assignee_username" elt)))))

(defun my-gitlab-status-vacation ()
  (interactive)
  (my-gitlab--set-status "palm_tree" "휴가")
  )

(defun my-gitlab-status-clear ()
  (interactive)
  (my-gitlab--set-status "" "")
  )

(defun my-gitlab--set-status (emoji message)
  (when (not (boundp 'gitlab-api-url)) (throw 'gitlab-api-url "not bound"))
  (when (not (boundp 'gitlab-private-key)) (throw 'gitlab-private-key "not bound"))
  (request
    (format "%s/user/status?private_token=%s" gitlab-api-url gitlab-private-key)
    :type "PUT"
    :data (json-encode `(("emoji" . ,emoji) ("message" . ,message)))
    :headers '(("Content-Type" . "application/json"))
    :sync t
    :parser 'json-read
    :encoding 'utf-8
    :complete (cl-function
               (lambda (&key response &allow-other-keys)
                 (message "Done: %s" (request-response-status-code response)))))
  )

(defun my-gitlab-open-issue-milestone (milestones)
  (when (not (boundp 'gitlab-team-members)) (throw 'gitlab-team-members "not bound"))
  (switch-to-buffer (make-temp-name "gitlab milestone issues"))
  (org-mode)
  (org-meta-return)
  (dolist (u gitlab-team-members)
    (insert u)
    (org-meta-return)
    (org-demote)
    (dolist (m milestones)
      (insert m)
      (let ((issues (my-gitlab--issues (list (list "assignee_username" u) (list "milestone" m)))))
        (when (> (length issues) 0)
          (org-meta-return)
          (org-demote)
          (dolist (i issues)
            (my-gitlab--insert-issues i)
            (org-meta-return))
          (org-promote)))
      (org-meta-return))
    (org-promote)))

(defun my-gitlab--insert-issues-by (params)
  (let ((issues (my-gitlab--issues params)))
    (dolist (elt issues)
      (org-meta-return)
      (my-gitlab--insert-issues elt))))

(defun my-gitlab--issues (params)
  (my-gitlab--request-get
   (my-gitlab--build-request gitlab-api-project-url
                             "issues"
                             (append params
                                     '(("state" "opened"))))))

(defun my-gitlab--build-request (api-url page properties)
  (when (not (boundp 'gitlab-private-key)) (throw 'gitlab-private-key "not bound"))
  (format "%s/%s?%s"
          api-url
          page
          (my-gitlab--build-params (cons `("private_token" ,gitlab-private-key) properties))))

(defun my-gitlab--build-params (params)
  (let (value '())
    (dolist (elt params value)
      (setq value
            (cons (format "%s=%s" (car elt) (cadr elt))
                  value)))
    (string-join value "&")))

(defun my-gitlab--request-get (url)
  (message (format "request get - %s" url))
  (let ((json-array-type 'list)
        (json-object-type 'plist)
        (json-key-type 'keyword))
    (with-temp-buffer
      (url-insert-file-contents url)
      (json-read))))

(defun my-gitlab--insert-issues (issue)
  (let* ((title (plist-get issue :title))
         (web_url (plist-get issue :web_url))
         (iid (plist-get issue :iid))
         (assignee (or (plist-get issue :assignee) '(:username "none")))
         (username (plist-get assignee :username)))
    (insert (format "%s - [%s] [[%s][#%s]]"
                    title
                    username
                    web_url
                    iid))))

(provide 'my-gitlab)
;;; my-gitlab.el ends here

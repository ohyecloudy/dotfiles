;;; my-gitlab.el --- gitlab helper package

(defcustom my/gitlab-hosts '()
  "Property List of giltab host
    Ecah element has the form '((:url MY_GITLAB :api-url MY_GITLAB))"
  :type '(plist)
  :group 'my-gitlab)

(defun my/gitlab-insert-heading-content ()
  (interactive)
  (let* ((url (my/gitlab--url-clipboard-or-prompt))
         (content (my/gitlab-merge-request url))
         (reference (plist-get content :reference))
         (title (plist-get content :title)))
    (org-insert-heading)
    (insert (format "%s %s [/]" reference title))
    (org-update-statistics-cookies nil)
    (org-set-property "URL" url)))

(defun my/gitlab-merge-request-title (url)
  (when-let (content (my/gitlab-merge-request url))
    (concat (plist-get content :reference) " " (plist-get content :title))))

(defun my/gitlab-merge-request (url)
  (when-let* ((host (my/gitlab--find-host url my/gitlab-hosts))
              (request-url (my/gitlab--merge-request-url url host))
              (header (my/gitlab--auth (plist-get host :url)))
              (content (my/gitlab--get-merge-request request-url header)))
    content))

(defun my/gitlab--url-clipboard-or-prompt ()
  (let ((clipboard-content (gui-get-selection 'CLIPBOARD)))
    (if (and clipboard-content (string-match-p "^http" clipboard-content))
        clipboard-content
      (read-string "Enter URL: "))))

(defun my/gitlab--merge-request-url (web-url host)
  (when-let* ((host-url (plist-get host :url))
              (host-api-url (plist-get host :api-url))
              (groups-mr-id (my/gitlab--extract-groups-and-mr-id web-url host-url)))
    (concat host-api-url
            "/projects/"
            (url-hexify-string (plist-get groups-mr-id :groups))
            "/merge_requests/"
            (int-to-string (plist-get groups-mr-id :mr-id)))))

(defun my/gitlab--auth (host-url)
  (when-let* ((url (my/gitlab--remove-protocol host-url))
              (private-token (my/gitlab--private-token url)))
    `("PRIVATE-TOKEN" . ,private-token)))

(defun my/gitlab--remove-protocol (url)
  (let* ((parsed (url-generic-parse-url url))
         (host (url-host parsed))
         (path (url-filename parsed)))
    (format "%s%s" host path)))

(defun my/gitlab--private-token (host)
  (when-let* ((found (auth-source-search
                      :host host
                      :requires '(:secret)))
              (first-found (nth 0 found))
              (private-token (funcall (plist-get first-found :secret))))
    private-token))

(defun my/gitlab--find-host (url hosts)
  (car (seq-filter (lambda (elem)
                     (let ((host-url (plist-get elem :url)))
                       (string-match-p (concat "^" (regexp-quote host-url)) url)))
                   hosts)))

(defun my/gitlab--get-merge-request (url auth-header)
  (let ((filter-fun (lambda (content)
                      (let* ((title (plist-get content :title))
                             (description (plist-get content :description))
                             (web-url (plist-get content :web_url))
                             (reference (plist-get (plist-get content :references) :full)))
                        (list :title title :reference reference :web-url web-url :description description)))))
    (my/gitlab--get url
                    auth-header
                    filter-fun)))

(defun my/gitlab--get (url authorization-header filter-fun)
  (let ((url-request-extra-headers `(,authorization-header
                                     ("Content-type" . "application/json; charset=utf-8")))
        (json-key-type 'keyword)
        (json-object-type 'plist))
    (with-temp-buffer
      (url-insert-file-contents url)
      (let ((content (json-read)))
        (funcall filter-fun content)))))

(defun my/gitlab--extract-groups-and-mr-id (url end_point)
  (let* ((url-without-endpoint (replace-regexp-in-string (concat "^" end_point) "" url))
         (url_parts (split-string url-without-endpoint "/-/"))
         (mr-id (my/gitlab--merge-request-id (cadr url_parts))))
    (list :groups (my/gitlab-remove-leading-slash (car url_parts)) :mr-id mr-id)))

(defun my/gitlab--merge-request-id (url)
  (save-match-data
    (if (string-match "/?merge_requests/\\([0-9]+\\)" url)
        (string-to-number (match-string 1 url)))))

(defun my/gitlab-remove-leading-slash (url)
  (replace-regexp-in-string "^/" "" url))

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

(defun insert-gitlab-mr-link (id)
  (interactive "nmerge request id: ")
  (insert (format "[[%s/merge_requests/%d][!%d]]" gitlab-base-url id id)))

(defun insert-gitlab-issue-link (project id)
  (interactive
   (list
    (completing-read "project: " (mapcar 'car gitlab-projects))
    (read-number "issue id: ")))
  (let* ((project-property (cdr (assoc project gitlab-projects)))
         (base-url (plist-get project-property :url)))
    (insert (format "[[%s/issues/%d][%s#%d]]" base-url id project id))))

(defun insert-gitlab-milestone-issues (milestone username)
  (let* ((url (milestore-issues-url milestone username))
         (issues (get-issues url))
         (index 0))
    (org-insert-heading-after-current)
    (org-demote-subtree)
    (insert "목록")
    (dolist (i issues)
      (if (= index 0)
          (progn (org-insert-heading-after-current)
                 (org-demote-subtree))
        (org-insert-heading-after-current))
      (incf index)
      (insert (format "#%s %s" (plist-get i :iid) (plist-get i :title)))
      (org-set-property "URL" (format "%s/issues/%d" gitlab-base-url (plist-get i :iid))))))

(defun milestore-issues-url (milestone username)
  (format "%s/issues?milestone=%s&assignee_username=%s&state=opened&private_token=%s"
          gitlab-api-base-url
          milestone
          username
          gitlab-private-key))

(defun get-issues (url)
  (let ((issues '())
        (json-array-type 'list)
        (json-object-type 'plist)
        (json-key-type 'keyword))
    (message (format "load - %s" url))
    (with-temp-buffer
      (url-insert-file-contents url)
      (let ((content (json-read)))
        (dolist (i content)
          (add-to-list 'issues
                       (list :iid
                             (plist-get i :iid)
                             :title
                             (plist-get i :title)
                             :labels
                             (plist-get i :labels))))))
    issues))

(defun parse-title (url)
  (let ((title ""))
    (with-temp-buffer
      (url-insert-file-contents url)
      (let* ((json-key-type 'string)
             (content (json-read)))
        (dolist (element content)
          (when (string= (car element) "title")
            (setq title (cdr element))))))
    title))

(defun insert-gitlab-mr (id)
  (interactive "nmerge request id: ")
  (insert-gitlab-mr-link id)
  (let ((url (format "%s/merge_requests/%d?private_token=%s"
                     gitlab-api-base-url
                     id
                     gitlab-private-key)))
    (insert (format " %s" (parse-title url)))))

(defun insert-gitlab-issue (project id)
  (interactive
   (list
    (completing-read "project: " (mapcar 'car gitlab-projects))
    (read-number "issue id: ")))
  (insert-gitlab-issue-link project id)
  (let* ((project-property (cdr (assoc project gitlab-projects)))
         (api-url (plist-get project-property :api-url))
         (url (format "%s/issues/%d?private_token=%s"
                      api-url
                      id
                      gitlab-private-key)))
    (insert (format " %s" (parse-title url)))))

(defun insert-gitlab-issue-plain (id)
  (interactive "nissue id: ")
  (let ((url (format "%s/issues/%d?private_token=%s"
                     gitlab-api-base-url
                     id
                     gitlab-private-key)))
    (insert (format "#%d %s" id (parse-title url)))))

(defun insert-gitlab-issue-heading (project id)
  (interactive
   (list
    (completing-read "project: " (mapcar #'car gitlab-projects))
    (read-number "issue id: ")))
  (let* ((project-property (cdr (assoc project gitlab-projects)))
         (api-url (plist-get project-property :api-url))
         (base-url (plist-get project-property :url))
         (request-url (format "%s/issues/%d?private_token=%s"
                              api-url
                              id
                              gitlab-private-key))
         (url (format "%s/issues/%d" base-url id)))
    (org-insert-heading)
    (insert (format "%s#%d %s [/]" project id (parse-title request-url)))
    (org-update-statistics-cookies nil)
    (org-set-property "URL" url)))

(defun my/mrs-url (project begin-date end-date page)
  (let* ((project-property (cdr (assoc project gitlab-projects)))
         (api-url (plist-get project-property :api-url))
         (options (format "order_by=updated_at&state=merged&scope=all&per_page=100&page=%d" page)))
    (format "%s/merge_requests?%s&updated_after=%s&updated_before=%s&private_token=%s"
            api-url
            options
            begin-date
            end-date
            gitlab-private-key)))

(defun my/mrs (project begin-date end-date)
  (let ((mrs '())
        (json-array-type 'list)
        (json-object-type 'plist)
        (json-key-type 'keyword))
    (dotimes (page 10) ;; 현명하게 그만둘 수 있는 방법이 있을텐데. 무식하게 10번 돈다 고고
      (with-temp-buffer
        (url-insert-file-contents (my/mrs-url project begin-date end-date (+ page 1)))
        (let ((content (json-read)))
          (dolist (mr content)
            (add-to-list 'mrs
                         (list :iid
                               (plist-get mr :iid)
                               :title
                               (plist-get mr :title)
                               :target_branch
                               (plist-get mr :target_branch)
                               :author
                               (plist-get mr :author)
                               :assignee
                               (plist-get mr :assignee)
                               :merged_at
                               (plist-get mr :merged_at)
                               :web_url
                               (plist-get mr :web_url)
                               ))))))
    mrs))

(defun my/mr-commits (project mr-iid)
  (let* ((project-property (cdr (assoc project gitlab-projects)))
         (api-url (plist-get project-property :api-url))(commits '())
         (json-array-type 'list)
         (json-object-type 'plist)
         (json-key-type 'keyword)
         (url (format "%s/merge_requests/%s/commits?private_token=%s"
                      api-url
                      mr-iid
                      gitlab-private-key)))
    (with-temp-buffer
      (url-insert-file-contents url)
      (let ((content (json-read)))
        (dolist (c content)
          (add-to-list 'commits
                       (list :id
                             (plist-get c :id)
                             :title
                             (plist-get c :title)
                             :author_name
                             (plist-get c :author_name)
                             :message
                             (plist-get c :message))))))
    commits))

(defun insert-mr-commits (project mr-iid)
  (let* ((project-property (cdr (assoc project gitlab-projects)))
         (base-url (plist-get project-property :url))(commits '())
         (commits (my/mr-commits project mr-iid)))
    (dolist (c commits)
      (insert (format "**** [[%s/merge_requests/%s/diffs?commit_id=%s][%s]] [%s] %s"
                      base-url
                      mr-iid
                      (plist-get c :id)
                      (substring (plist-get c :id) 0 10)
                      (plist-get c :author_name)
                      (plist-get c :title)))
      (insert "\n")
      (insert "     #+BEGIN_QUOTE\n")
      (insert (replace-regexp-in-string "^.*?" "     " (plist-get c :message)))
      (insert "#+END_QUOTE\n"))))

(defun name (plist)
  (if (eq plist nil)
      "none"
    (plist-get plist :name)))

(defun my/insert-gitlab-mrs (project begin-date end-date)
  (let* ((begin-date (format "%sT00:00:00.000+09:00" begin-date))
         (end-date (format "%sT00:00:00.000+09:00" end-date))
         (mrs (my/mrs project begin-date end-date))
         (mrs (seq-filter (lambda (x)
                            (and
                             (string= (plist-get x :target_branch) "master")
                             (between-date-p begin-date end-date (plist-get x :merged_at))))
                          mrs)))
    (dolist (mr mrs)
      (let ((id (plist-get mr :iid)))
        (insert "*** TODO ")
        (insert (format "!%d" id))
        (insert (format " [%s - %s] %s"
                        (name (plist-get mr :author))
                        (name (plist-get mr :assignee))
                        (plist-get mr :title)))
        (org-return)
        (org-set-property "URL" (plist-get mr :web_url))
        (insert-mr-commits project (plist-get mr :iid))))))

(defun between-date-p (begin end target)
  (let ((begin (parse-iso8601-time-string begin))
        (end (parse-iso8601-time-string end))
        (target (parse-iso8601-time-string target)))
    (and (>= (time-subtract target begin) 0)
         (>= (time-subtract end target) 0))))

(defun my/commits-url (begin-date end-date page)
  (let ((before (format "%sT00:00:00.000%%2B09:00" end-date))
        (after (format "%sT00:00:00.000%%2B09:00" begin-date)))
    (format "%s/repository/commits?page=%d&per_page=100&since=%s&until=%s&private_token=%s"
            gitlab-api-base-url
            page
            after
            before
            gitlab-private-key)))

(defun my/commits (begin-date end-date)
  (let ((commits '())
        (json-array-type 'list)
        (json-object-type 'plist)
        (json-key-type 'keyword))
    (dotimes (page 10) ;; 현명하게 그만둘 수 있는 방법이 있을텐데. 무식하게 10번 돈다 고고
      (with-temp-buffer
        (url-insert-file-contents (my/commits-url begin-date end-date (+ page 1)))
        (let ((content (json-read)))
          (dolist (commit content)
            (add-to-list 'commits
                         (list :id
                               (plist-get commit :id)
                               :title
                               (plist-get commit :title)
                               :message
                               (plist-get commit :message)
                               :author_name
                               (plist-get commit :author_name)))))))
    commits))

(defun my/merge-request-url-from-commit (commit-id)
  (format "%s/repository/commits/%s/merge_requests?private_token=%s"
          gitlab-api-base-url
          commit-id
          gitlab-private-key))

(defun my/commits-without-merge-request (begin-date end-date)
  (let ((ret '())
        (json-array-type 'list)
        (source-commits (my/commits begin-date end-date)))
    (message (format "total commit count - %d" (length source-commits)))
    (dolist (c source-commits)
      ;; gitlab이 만든 merge 커밋은 연관된 merge request 정보가 없다.
      ;; merge request 번호를 적은 커밋 메시지를 찾아서 걸러낸다
      (unless (string-match "See merge request"
                            (plist-get c :message))
        (with-temp-buffer
          (url-insert-file-contents (my/merge-request-url-from-commit (plist-get c :id)))
          (let ((json-key-type 'string)
                (content (json-read)))
            (when (eq 0 (length content))
              (add-to-list 'ret c))))))
    ret))

(defun insert-gitlab-commit-link (id)
  (insert (format "[[%s/commit/%s][%s]]" gitlab-base-url id (substring id 0 10))))

(defun my/insert-gitlab-commits-without-mr (begin-date end-date)
  (let ((source-commits (my/commits-without-merge-request begin-date end-date)))
    (dolist (c source-commits)
      (insert "*** TODO ")
      (insert-gitlab-commit-link (plist-get c :id))
      (insert (format " [%s] %s" (plist-get c :author_name) (plist-get c :title)))
      (insert "\n"))))

(defun insert-gitlab-commits-without-mr-range ()
  (interactive)
  (let ((begin-date (org-read-date))
        (end-date (org-read-date)))
    (my/insert-gitlab-commits-without-mr begin-date end-date)))

(defun insert-gitlab-todo-codereview (project)
  (interactive
   (list
    (completing-read "project: " (mapcar 'car gitlab-projects))))
  (let ((begin-date (org-read-date))
        (end-date (org-read-date)))
    (my/insert-gitlab-mrs project begin-date end-date)))

(provide 'legacy-gitlab)

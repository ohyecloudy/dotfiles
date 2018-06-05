(when (and (boundp 'gitlab-private-key)
           (boundp 'gitlab-api-base-url)
           (boundp 'gitlab-base-url))

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
                                 (plist-get commit :message)))))))
      (reverse commits)))

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

  (defun insert-gitlab-commits-without-mr-range ()
    (interactive)
    (let* ((begin-date (org-read-date))
           (end-date (org-read-date))
           (source-commits (my/commits-without-merge-request begin-date end-date)))
      (dolist (c source-commits)
        (insert "*** TODO ")
        (insert-gitlab-commit-link (plist-get c :id))
        (insert (format " %s" (plist-get c :title)))
        (insert "\n")))))

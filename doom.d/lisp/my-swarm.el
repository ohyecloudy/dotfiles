;;; lisp/my-swarm.el -*- lexical-binding: t; -*-

(defun my/swarm-insert-code-reviews ()
  (interactive)
  (let* ((begin-date (my/swarm--org-read-date-to-iso8601 (org-read-date)))
         (end-date (my/swarm--org-read-date-to-iso8601 (org-read-date)))
         (username (my/swarm--get-secret "username"))
         (ticket (my/swarm--get-secret "ticket"))
         (host (my/swarm--get-secret "host"))
         (project (my/swarm--get-secret "project"))
         (reviews (my/swarm--fetch-reviews username ticket host project begin-date end-date)))
    (message "fetched reviews. begin: %S after: %S count: %d" begin-date end-date (length reviews))
    (dolist (r reviews)
      (let ((web-url (format "%s/reviews/%s" host (plist-get r :id))))
        (insert "*** TODO ")
        (insert (format "%s" (plist-get r :id)))
        (insert (format " [%s] %s"
                        (plist-get r :author)
                        (plist-get r :title)))
        (org-return)
        (org-set-property "URL" web-url)
        (org-return)
        (insert "     #+BEGIN_QUOTE\n")
        (insert (replace-regexp-in-string "^.*?" "     " (plist-get r :description)))
        (insert "#+END_QUOTE\n")
        (org-return)))))

(defun my/swarm--fetch-reviews (username ticket host project begin-date end-date)
  (let* ((base64-encoded (base64url-encode-string (format "%s:%s" username ticket)))
         (basic-authorization (format "Basic %s" base64-encoded))
         (api-url (format "%s/api/v11/reviews?max=500&project[]=%s" host project))
         (url-request-extra-headers `(("Authorization" . ,basic-authorization)
                                      ("Content-type" . "application/json; charset=utf-8")))
         (json-key-type 'keyword)
         (json-object-type 'plist)
         (reviews '()))
    (with-temp-buffer
      (url-insert-file-contents api-url)
      (let ((content (json-read)))
        (dolist (c content)
          (seq-doseq (r (plist-get c :reviews))
            (when (my/swarm--between-date-p begin-date
                                            end-date
                                            (my/swarm--to-iso8601 (plist-get r :created)))
              (add-to-list 'reviews
                           (list :id (plist-get r :id)
                                 :author (plist-get r :author)
                                 :title (my/swarm--extract-title (plist-get r :description))
                                 :description (plist-get r :description))))))))
    reviews))

(defun my/swarm--extract-title (desc)
  (car (split-string desc "\n")))

(defun my/swarm--org-read-date-to-iso8601 (date)
  (format "%sT00:00:00.000+09:00" date))

(defun my/swarm--get-secret (key)
  (funcall
   (plist-get
    (nth 0 (auth-source-search
            :host "myswarm"
            :user key
            :requires '(:secret)))
    :secret))
  )

(defun my/swarm--to-iso8601 (unix-time)
  (format-time-string "%FT%T%z" (seconds-to-time unix-time)))

(defun my/swarm--between-date-p (begin end target)
  (let ((begin (parse-iso8601-time-string begin))
        (end (parse-iso8601-time-string end))
        (target (parse-iso8601-time-string target)))
    (and (>= (time-subtract target begin) 0)
         (>= (time-subtract end target) 0))))


(provide 'my-swarm)

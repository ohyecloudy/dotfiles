;;; my-gitlab.el --- gitlab helper package

(defun my-gitlab-open-todo ()
  (interactive)
  (let ((todos (my-gitlab--request-get (my-gitlab--build-request
                                        "todos"
                                        '(("per_page" 10))))))
    (dolist (elt todos)
      (browse-url (plist-get elt :target_url)))))

(defun my-gitlab--build-request (page properties)
  (when (not (boundp 'gitlab-private-key)) (throw 'gitlab-private-key "not bound"))
  (when (not (boundp 'gitlab-api-url)) (throw 'gitlab-api-url "not bound"))
  (format "%s/%s?%s"
          gitlab-api-url
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
  (let ((json-array-type 'list)
        (json-object-type 'plist)
        (json-key-type 'keyword))
    (with-temp-buffer
      (url-insert-file-contents url)
      (json-read))))

(provide 'my-gitlab)
;;; my-gitlab.el ends here

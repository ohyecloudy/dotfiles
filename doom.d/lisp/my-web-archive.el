;;; my-web-archive.el --- Library to retrieve web archive URLs  -*- lexical-binding: t; -*-

;; Copyright (C) 2024 Jongbin Oh

;; Author: Jongbin Oh <ohyecloudy@gmail.com>
;; Created: 2024-03-29

(defconst my/web-archive-result-buffer-name "*my/web-archive-result*")
(defconst my/web-archive-log-buffer-name "*my/web-archive-log*")

(when (functionp #'set-popup-rule!)
  (set-popup-rule! my/web-archive-result-buffer-name :size 0.25 :ttl nil :vslot -1))

(defun my/web-archive-async (urls &optional result-func)
  (unless (and (listp urls) urls)
    (error "urls should be non empty list"))

  (let ((total (length urls))
        (count 0)
        (result-func (or result-func #'my/web-archive--process-result)))

    (my/web-archive--logger (format "web archive start... %S urls" total))
    (my/web-archive--insert-separator-to-result-buffer)

    (defun process-sentinel (proc event)
      (when (string-equal event "finished\n")
        (setq count (1+ count))
        (if (>= count total)
            (progn
              (my/web-archive--logger "all web archives done!")
              (my/web-archive--display-result-buffer))
          (my/web-archive--logger (format "progress %S/%S" count total)))))

    (dolist (u urls)
      (let ((proc (my/web-archive--start-async-request
                   u
                   #'my/web-archive--logger
                   result-func)))
        (set-process-sentinel proc #'process-sentinel)))))

(defun my/web-archive-write-line (buffer-name message)
  (with-current-buffer (get-buffer-create buffer-name)
    ;; 명시적으로 호출하지 않으면 evil-mode가 활성화되지 않는다.
    ;; 정확한 원인은 모르며 workaround로 fundamental-mode 함수를 호출.
    (fundamental-mode)
    (setq buffer-read-only t)
    (goto-char (point-max))
    (let ((inhibit-read-only t))
      (save-excursion
        (insert (format "%s\n" message))))))

(defun my/web-archive--start-async-request (url logger result-func)
  (let* ((start-time (current-time))
         (proc (apply #'start-process
                      (append (list "web-archive" nil)
                              (my/web-archive--build-curl-commands url)))))
    (funcall logger (format "request web archive %S" url))
    (set-process-filter proc (lambda (proc output)
                               (let ((elapsed (float-time (time-subtract (current-time) start-time)))
                                     (web-archive-url (my/web-archive--extract-redirect-url output)))
                                 (funcall result-func url web-archive-url)
                                 (funcall logger
                                          (format "url: %S\nresult: %S\nelapsed: %Ssec"
                                                  url
                                                  web-archive-url elapsed)))))
    proc))

(defun my/web-archive--build-curl-commands (url)
  (let* ((curl-executable (executable-find "curl"))
         (request-url (format "http://web.archive.org/save/%s" url)))
    (unless curl-executable
      (error "cannot find 'curl' executable path"))
    `(,curl-executable "--silent" "--head" ,request-url)))

(defun my/web-archive--extract-redirect-url (str)
  ;; replace ANSI escape codes and ^M with an empty string
  (let ((str (replace-regexp-in-string "\033\\[[0-9;]*m\\|\033\\]8;.*?\033\\\\\\|\r+$" "" str)))
    (save-match-data
      (if (string-match "location: \\(.*\\)" str)
          (match-string 1 str)))))

(defun my/web-archive--process-result (url archive-url)
  (my/web-archive-write-line my/web-archive-result-buffer-name
                             (format "%s [[%s][archive]]\n" url archive-url)))

(defun my/web-archive--logger (message)
  (my/web-archive-write-line my/web-archive-log-buffer-name
                             message))

(defun my/web-archive--insert-separator-to-result-buffer ()
  (my/web-archive-write-line my/web-archive-result-buffer-name
                             "================================================================================"))

(defun my/web-archive--display-result-buffer ()
  (with-current-buffer (get-buffer-create my/web-archive-result-buffer-name)
    (display-buffer (current-buffer))))

(provide 'my-web-archive)
;;; my-web-archive.el ends here

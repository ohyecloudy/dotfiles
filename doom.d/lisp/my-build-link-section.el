;;; my-build-link-section.el --- Collect weblinks from org documents to create link -*- lexical-binding: t; -*-

;; Copyright (C) 2024 Jongbin Oh

;; Author: Jongbin Oh <ohyecloudy@gmail.com>

(require 'my-org-cliplink)

(defun my/build-link-section (&optional link-inserter-func)
  (interactive)
  (let ((links (sort
                (delete-dups (my/build-link-section--extract-urls (org-element-parse-buffer)))
                'string<))
        (link-inserter-func (or link-inserter-func #'my/build-link-section--default-inserter)))
    (funcall link-inserter-func links)))

(defun my/build-link-section--default-inserter (links)
  (org-insert-heading-after-current)
  (insert "링크")
  (org-return t)
  (org-return t)
  (seq-map-indexed (lambda (elt idx)
                     (message (format "processing - %s" elt))
                     (let* ((url (url-encode-url elt))
                            (title (or (org-cliplink-retrieve-title-synchronously url)
                                       "nil"))
                            (link-elt (my/org-cliplink-link-transformer url title)))
                       ;; 첫번째 요소는 직접 정렬되지 않은 목록 아이템을 넣어준다
                       (if (= idx 0)
                           (progn
                             (insert (format "- %s" link-elt))
                             (org-return t))
                         ;; 두번째 요소 부터는 org-insert-item 함수를 호출해
                         ;; 이전 목록 아이템을 참고해 자동으로 넣는다
                         (progn
                           (org-insert-item)
                           (insert link-elt)
                           (org-return t)))))
                   links))

(defun my/build-link-section--extract-urls (org-elements)
  ;; link 타입 org element만 map
  (org-element-map org-elements 'link
    (lambda (link)
      (let* ((link-part (nth 1 link))
             (type (plist-get link-part :type))
             (path (url-unhex-string (plist-get link-part :raw-link))))
        ;; "https", "http"로 시작하는 link만 골라낸다
        (if (or (string= type "https") (string= type "http"))
            ;; "https://...", "http://..." 같은 전체 주소
            path)))))

(provide 'my-build-link-section)
;;; my-build-link-section.el ends here

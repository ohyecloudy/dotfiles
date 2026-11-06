;;; my-ox-gfm-link.el --- Clean up links on GFM export  -*- lexical-binding: t; -*-

;; Makes GFM export emit bare web URLs so pasted text turns into smart links
;; (e.g. Jira/Confluence) instead of markdown links or <autolinks>.
;; Org-roam id links are reduced to their description since the targets
;; are unreachable outside the local notes.

(require 'org)

(declare-function org-element-property "org-element")
(declare-function org-export-derived-backend-p "ox")
(declare-function org-export-inline-image-p "ox")
(defvar org-html-inline-image-rules)

(defgroup my-ox-gfm-link nil
  "Link cleanup for GFM export."
  :group 'org-export)

(defcustom my/ox-gfm-link-url-only-prefixes '()
  "URL prefixes whose links are exported as bare URLs, dropping descriptions."
  :type '(repeat string)
  :group 'my-ox-gfm-link)

(defconst my/ox-gfm-link--web-types '("http" "https")
  "Link types handled by the cleanup.")

(defconst my/ox-gfm-link--id-type "id"
  "Link type of org-roam node links, exported as description only.")

(defun my/ox-gfm-link--url-only-p (url)
  "Return non-nil if URL starts with one of `my/ox-gfm-link-url-only-prefixes'."
  (seq-some (lambda (prefix) (string-prefix-p prefix url))
            my/ox-gfm-link-url-only-prefixes))

(defun my/ox-gfm-link--around (fn link desc info)
  "Around advice for `org-md-link' that cleans up links on GFM export.
FN is the original function, LINK, DESC and INFO are its arguments."
  (let ((type (org-element-property :type link))
        (gfm-p (org-export-derived-backend-p (plist-get info :back-end) 'gfm)))
    (cond
     ((and gfm-p
           (string= type my/ox-gfm-link--id-type)
           (org-string-nw-p desc))
      desc)
     ((and gfm-p
           (member type my/ox-gfm-link--web-types)
           (not (org-export-inline-image-p link org-html-inline-image-rules)))
      (let ((url (concat type ":" (org-element-property :path link))))
        (if (or (not (org-string-nw-p desc))
                (my/ox-gfm-link--url-only-p url))
            url
          (funcall fn link desc info))))
     (t (funcall fn link desc info)))))

(advice-add 'org-md-link :around #'my/ox-gfm-link--around)

(provide 'my-ox-gfm-link)

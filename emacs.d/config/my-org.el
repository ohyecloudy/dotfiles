;;; org-journal // https://github.com/bastibe/org-journal
(setq org-journal-dir "~/journal/")
(setq org-journal-date-format "%Y-%m-%d, %a")
(setq org-journal-date-prefix "#+TITLE: ")
(setq org-journal-time-format "")
(setq org-journal-time-prefix "")

(setq org-adapt-indentation nil)

;;; image
(setq org-startup-with-inline-images t)

;;; plantuml
(org-babel-do-load-languages
 'org-babel-load-languages
 '((plantuml . t)))
(setq org-confirm-babel-evaluate nil)
(setq org-plantuml-jar-path
      (expand-file-name "~/bin/plantuml.jar"))
(add-hook 'org-babel-after-execute-hook
          (lambda ()
            (when org-inline-image-overlays
              (org-redisplay-inline-images))))
(add-to-list 'org-structure-template-alist
             '("u" "#+BEGIN_SRC plantuml :file ?.png\nskinparam monochrome true\n#+END_SRC"))

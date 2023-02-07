;;; lisp/my-source-mine.el -*- lexical-binding: t; -*-

(defun my/source-mine-csharp ()
  (interactive)
  (my/source-mine--do "csharp"))

(defun my/source-mine-elixir ()
  (interactive)
  (my/source-mine--do "elixir"))

(defun my/source-mine--do (lang)
  (let ((search-terms (my/source-mine--use-region-or-empty))
        (dir (my/source-mine--dir lang)))
    (+vertico/project-search t search-terms dir)))

(defun my/source-mine--use-region-or-empty ()
  (if (doom-region-active-p)
      (buffer-substring (doom-region-beginning) (doom-region-end))
    ""))

;; machine source_mine login csharp password ~/source_mine/csharp
;; machine source_mine login elixir password ~/source_mine/elixir
;; 이런 식으로 ~/authinfo 파일에 저장하면 된다.
(defun my/source-mine--dir (lang)
  (funcall
   (plist-get
    (nth 0 (auth-source-search
            :host "source_mine"
            :user lang
            :requires '(:secret)))
    :secret))
  )

(provide 'my-source-mine)
;;; my-source-mine.el ends here

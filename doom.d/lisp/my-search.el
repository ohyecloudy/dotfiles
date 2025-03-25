;;; lisp/my-search.el -*- lexical-binding: t; -*-

(defun my/search-blogs ()
  (interactive)
  (let ((search-terms (format "site:ohyecloudy.com %s"
                              (my/search--use-region-or-read-user-input))))
    (my/search--browse "https://google.com/search"
                       `(("q" ,search-terms)))
    )
  )

(defun my/search-google ()
  (interactive)
  (let ((search-terms (my/search--use-region-or-read-user-input)))
    (my/search--browse "https://google.com/search"
                       `(("q" ,search-terms)))
    )
  )

(defun my/search-naver ()
  (interactive)
  (let ((search-terms (my/search--use-region-or-read-user-input)))
    (my/search--browse "https://search.naver.com/search.naver"
                       `(("query" ,search-terms)))
    )
  )

(defun my/search-dict ()
  (interactive)
  (let ((search-terms (my/search--use-region-or-read-user-input)))
    (my/search--browse "https://dict.naver.com/search.dict"
                       `(("query" ,search-terms)))
    )
  )

(defun my/search-dotnet ()
  (interactive)
  (let ((search-terms (my/search--use-region-or-read-user-input)))
    (my/search--browse "https://learn.microsoft.com/ko-kr/search/"
                       `(("terms" ,search-terms)
                         ("scope" ".NET")))
    )
  )

(defun my/search-elixir ()
  (interactive)
  (let ((search-terms (my/search--use-region-or-read-user-input)))
    (my/search--browse "https://hexdocs.pm/elixir/search.html"
                       `(("q" ,search-terms)))
    )
  )

(defun my/search-flutter ()
  (interactive)
  (let ((search-terms (my/search--use-region-or-read-user-input)))
    (my/search--browse "https://docs.flutter.dev/search"
                       `(("q" ,search-terms)))
    )
  )

(defun my/search-onelook ()
  (interactive)
  (let ((search-terms (my/search--use-region-or-read-user-input)))
    (my/search--browse "https://onelook.com"
                       `(("w" ,search-terms)))
    )
  )

(defun my/search-core-dictionary ()
  (interactive)
  (let ((search-terms (my/search--use-region-or-read-user-input)))
    (my/search--browse "http://www.coredictionary.com/core"
                       `(("w" ,search-terms)))
    )
  )

(defun my/search-thesaurus ()
  (interactive)
  (let* ((search-terms (my/search--use-region-or-read-user-input))
         (url (format "https://www.thesaurus.com/browse/%s" search-terms)))
    (browse-url url)
    )
  )

(defun my/search-perplexity ()
  (interactive)
  (let ((search-terms (my/search--use-region-or-read-user-input)))
    (my/search--browse "https://perplexity.ai/search"
                       `(("q" ,search-terms)))
    )
  )

(defun my/search--browse (base-url query-string-list)
  (let* ((query-string (url-build-query-string query-string-list))
         (url (format "%s?%s" base-url query-string)))
    (browse-url url))
  )

(defun my/search--use-region-or-read-user-input ()
  (if (doom-region-active-p)
      (buffer-substring (doom-region-beginning) (doom-region-end))
    (read-string "Enter your search terms: ")))

(provide 'my-search)
;;; my-search.el ends here

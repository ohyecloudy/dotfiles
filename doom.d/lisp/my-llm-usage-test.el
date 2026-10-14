;;; my-llm-usage-test.el --- Tests for my-llm-usage  -*- lexical-binding: t; -*-

(require 'ert)
(require 'cl-lib)
(require 'my-llm-usage)

(defconst my/llm-usage-test--body
  "{\"key_name\":\"sk-...abcd\",
    \"llm_usage\":{\"budget\":500.0,\"total_spend\":363.23,
                   \"daily_usage\":{\"spend\":36.66},
                   \"budget_reset_at\":\"2026-11-01T12:00:00+00:00\",
                   \"remaining_budget_reset_in_days\":18},
    \"llm_usage_updated_at\":\"2026-10-14T17:19:51+09:00\"}"
  "Portal response trimmed to the fields the package reads.")

(defun my/llm-usage-test--usage (&rest overrides)
  "Return the parsed test usage alist with OVERRIDES (KEY VALUE ...) applied."
  (let ((usage (copy-alist (car (my/llm-usage-parse my/llm-usage-test--body)))))
    (while overrides
      (setf (alist-get (pop overrides) usage) (pop overrides)))
    usage))

(defmacro my/llm-usage-test--with-clean-state (&rest body)
  "Run BODY with fresh package state and no hooks."
  (declare (indent 0))
  `(let ((my/llm-usage-latest nil)
         (my/llm-usage-updated-at nil)
         (my/llm-usage-last-error nil)
         (my/llm-usage-updated-hook nil)
         (my/llm-usage--last-request-time nil)
         (my/llm-usage--in-flight nil))
     ,@body))

(defmacro my/llm-usage-test--with-portal (url key response &rest body)
  "Run BODY with portal URL, gateway KEY and a fake `url-queue-retrieve'.
RESPONSE is (STATUS . RAW-HTTP-TEXT) passed to the callback, or nil to
never call back.  Requested URLs are collected in `requested'."
  (declare (indent 3))
  `(let ((requested nil)
         (process-environment (cons (concat "ANTHROPIC_API_KEY=" (or ,key ""))
                                    process-environment)))
     (cl-letf (((symbol-function 'my/llm-usage--portal-url) (lambda () ,url))
               ((symbol-function 'url-queue-retrieve)
                (lambda (request-url callback &rest _)
                  (push request-url requested)
                  (when-let* ((response ,response))
                    (with-current-buffer (generate-new-buffer " *llm-usage-test*")
                      (insert (cdr response))
                      (funcall callback (car response)))))))
       ,@body)))

;;; Parsing

(ert-deftest my/llm-usage-test/parse-valid ()
  (let ((result (my/llm-usage-parse my/llm-usage-test--body)))
    (should (equal 363.23 (alist-get 'total_spend (car result))))
    (should (equal "2026-10-14T17:19:51+09:00" (cdr result)))))

(ert-deftest my/llm-usage-test/parse-missing-usage ()
  (should-error (my/llm-usage-parse "{\"detail\":\"denied\"}")))

(ert-deftest my/llm-usage-test/parse-not-json ()
  (should-error (my/llm-usage-parse "<html>bad gateway</html>")))

(ert-deftest my/llm-usage-test/parse-non-object ()
  (should-error (my/llm-usage-parse "[1,2]")))

;;; Formatting

(ert-deftest my/llm-usage-test/format-text ()
  (should (equal "$363.2/$500 (73%) 18d"
                 (substring-no-properties
                  (my/llm-usage-format (my/llm-usage-test--usage) nil nil)))))

(ert-deftest my/llm-usage-test/format-normal-face ()
  (should (eq 'shadow
              (get-text-property
               0 'face (my/llm-usage-format (my/llm-usage-test--usage) nil nil 'shadow)))))

(ert-deftest my/llm-usage-test/format-warning-face ()
  (should (eq 'warning
              (get-text-property
               0 'face (my/llm-usage-format
                        (my/llm-usage-test--usage 'total_spend 400.0) nil nil 'shadow)))))

(ert-deftest my/llm-usage-test/format-no-budget ()
  (should (equal "$363.2 18d"
                 (substring-no-properties
                  (my/llm-usage-format (my/llm-usage-test--usage 'budget nil) nil nil))))
  (should (equal "$363.2 18d"
                 (substring-no-properties
                  (my/llm-usage-format (my/llm-usage-test--usage 'budget 0) nil nil)))))

(ert-deftest my/llm-usage-test/format-no-days ()
  (should (equal "$363.2/$500 (73%)"
                 (substring-no-properties
                  (my/llm-usage-format
                   (my/llm-usage-test--usage 'remaining_budget_reset_in_days nil) nil nil)))))

(ert-deftest my/llm-usage-test/format-help-echo ()
  (let ((echo (get-text-property
               0 'help-echo
               (my/llm-usage-format (my/llm-usage-test--usage)
                                    "2026-10-14T17:19:51+09:00" "boom"))))
    (should (string-match-p "^today \\$36\\.66$" echo))
    (should (string-match-p "^resets 11-01 " echo))
    (should (string-match-p "^updated [0-9][0-9]:[0-9][0-9]$" echo))
    (should (string-match-p "^last refresh failed: boom$" echo))))

(ert-deftest my/llm-usage-test/format-invalid-times-omitted ()
  (let ((echo (get-text-property
               0 'help-echo
               (my/llm-usage-format (my/llm-usage-test--usage 'budget_reset_at "soon")
                                    "later" nil))))
    (should (equal "today $36.66" echo))))

(ert-deftest my/llm-usage-test/format-error-only ()
  (let ((text (my/llm-usage-format nil nil "boom")))
    (should (equal "$?" (substring-no-properties text)))
    (should (eq 'warning (get-text-property 0 'face text)))))

(ert-deftest my/llm-usage-test/format-nothing ()
  (should-not (my/llm-usage-format nil nil nil)))

;;; Request URL

(ert-deftest my/llm-usage-test/request-url-encodes-key ()
  (should (equal "https://portal/v1/usage/me?key=sk-a%2Bb"
                 (my/llm-usage--request-url "https://portal/v1/usage/me" "sk-a+b")))
  (should (equal "https://portal/me?x=1&key=k"
                 (my/llm-usage--request-url "https://portal/me?x=1" "k"))))

;;; Refresh

(ert-deftest my/llm-usage-test/refresh-success ()
  (my/llm-usage-test--with-clean-state
    (let ((hook-runs 0))
      (add-hook 'my/llm-usage-updated-hook (lambda () (cl-incf hook-runs)))
      (my/llm-usage-test--with-portal "https://portal/me" "sk-test"
          (cons nil (concat "HTTP/1.1 200 OK\r\nContent-Type: application/json\r\n\r\n"
                            my/llm-usage-test--body))
        (should (my/llm-usage-refresh))
        (should (equal '("https://portal/me?key=sk-test") requested))
        (should (equal 363.23 (alist-get 'total_spend my/llm-usage-latest)))
        (should (equal "2026-10-14T17:19:51+09:00" my/llm-usage-updated-at))
        (should-not my/llm-usage-last-error)
        (should-not my/llm-usage--in-flight)
        (should (= 1 hook-runs))))))

(ert-deftest my/llm-usage-test/refresh-http-error-keeps-latest ()
  (my/llm-usage-test--with-clean-state
    (setq my/llm-usage-latest '((total_spend . 1.0)))
    (my/llm-usage-test--with-portal "https://portal/me" "sk-test"
        (cons '(:error (error http 422))
              "HTTP/1.1 422 Unprocessable\r\n\r\n{}")
      (my/llm-usage-refresh)
      (should (equal '((total_spend . 1.0)) my/llm-usage-latest))
      (should (string-match-p "422" my/llm-usage-last-error))
      (should-not (string-match-p "sk-test" my/llm-usage-last-error))
      (should-not my/llm-usage--in-flight))))

(ert-deftest my/llm-usage-test/refresh-invalid-body ()
  (my/llm-usage-test--with-clean-state
    (my/llm-usage-test--with-portal "https://portal/me" "sk-test"
        (cons nil "HTTP/1.1 200 OK\r\n\r\n<html>oops</html>")
      (my/llm-usage-refresh)
      (should-not my/llm-usage-latest)
      (should (string-match-p "^invalid response" my/llm-usage-last-error)))))

(ert-deftest my/llm-usage-test/refresh-kills-response-buffer ()
  (my/llm-usage-test--with-clean-state
    (let ((before (length (buffer-list))))
      (my/llm-usage-test--with-portal "https://portal/me" "sk-test"
          (cons nil (concat "HTTP/1.1 200 OK\r\n\r\n" my/llm-usage-test--body))
        (my/llm-usage-refresh))
      (should (= before (length (buffer-list)))))))

(ert-deftest my/llm-usage-test/refresh-throttled ()
  (my/llm-usage-test--with-clean-state
    (my/llm-usage-test--with-portal "https://portal/me" "sk-test"
        (cons nil (concat "HTTP/1.1 200 OK\r\n\r\n" my/llm-usage-test--body))
      (should (my/llm-usage-refresh))
      (should-not (my/llm-usage-refresh))
      (should (my/llm-usage-refresh t))
      (should (= 2 (length requested))))))

(ert-deftest my/llm-usage-test/refresh-skips-while-in-flight ()
  (my/llm-usage-test--with-clean-state
    (my/llm-usage-test--with-portal "https://portal/me" "sk-test" nil
      (should (my/llm-usage-refresh))
      (should my/llm-usage--in-flight)
      (should-not (my/llm-usage-refresh t))
      (should (= 1 (length requested))))))

(ert-deftest my/llm-usage-test/refresh-without-url-does-nothing ()
  (my/llm-usage-test--with-clean-state
    (my/llm-usage-test--with-portal nil "sk-test" nil
      (should-not (my/llm-usage-refresh t))
      (should-not requested)
      (should-not my/llm-usage-last-error))))

(ert-deftest my/llm-usage-test/refresh-without-key-reports-error ()
  (my/llm-usage-test--with-clean-state
    (let ((process-environment (cons "ANTHROPIC_AUTH_TOKEN" process-environment)))
      (my/llm-usage-test--with-portal "https://portal/me" nil nil
        (should-not (my/llm-usage-refresh t))
        (should-not requested)
        (should (string-match-p "^no key in ANTHROPIC_API_KEY"
                                my/llm-usage-last-error))))))

(provide 'my-llm-usage-test)
;;; my-llm-usage-test.el ends here

;;; my-org-formatter-test.el --- Tests for my-org-formatter  -*- lexical-binding: t; -*-

(require 'ert)
(require 'org)
(require 'my-org-formatter)

(defun my/org-formatter-test--run (input expected)
  "Run `my/org-formatter-enforce' on INPUT, compare with EXPECTED."
  (with-temp-buffer
    (org-mode)
    (insert input)
    (my/org-formatter-enforce)
    (should (string= expected (buffer-string)))))

;;; Before heading

(ert-deftest my/org-formatter-test/no-blank-before ()
  (my/org-formatter-test--run
   "text\n* H1\n"
   "text\n\n* H1\n"))

(ert-deftest my/org-formatter-test/multiple-blanks-before ()
  (my/org-formatter-test--run
   "text\n\n\n\n* H1\n"
   "text\n\n* H1\n"))

(ert-deftest my/org-formatter-test/first-heading-at-bob ()
  (my/org-formatter-test--run
   "* H1\n\nbody\n"
   "* H1\n\nbody\n"))

;;; After heading

(ert-deftest my/org-formatter-test/no-blank-after ()
  (my/org-formatter-test--run
   "* H1\nbody\n"
   "* H1\n\nbody\n"))

(ert-deftest my/org-formatter-test/multiple-blanks-after ()
  (my/org-formatter-test--run
   "* H1\n\n\n\nbody\n"
   "* H1\n\nbody\n"))

(ert-deftest my/org-formatter-test/heading-at-eob ()
  (my/org-formatter-test--run
   "text\n\n* H1\n"
   "text\n\n* H1\n"))

;;; Already correct

(ert-deftest my/org-formatter-test/already-correct ()
  (my/org-formatter-test--run
   "text\n\n* H1\n\nbody\n"
   "text\n\n* H1\n\nbody\n"))

;;; Consecutive headings

(ert-deftest my/org-formatter-test/consecutive-headings ()
  (my/org-formatter-test--run
   "* H1\n* H2\n"
   "* H1\n\n* H2\n"))

(ert-deftest my/org-formatter-test/consecutive-headings-multiple-blanks ()
  (my/org-formatter-test--run
   "* H1\n\n\n* H2\n"
   "* H1\n\n* H2\n"))

;;; Planning and drawers

(ert-deftest my/org-formatter-test/with-planning ()
  (my/org-formatter-test--run
   "* TODO H1\nSCHEDULED: <2026-05-13>\nbody\n"
   "* TODO H1\nSCHEDULED: <2026-05-13>\n\nbody\n"))

(ert-deftest my/org-formatter-test/with-drawer ()
  (my/org-formatter-test--run
   "* H1\n:PROPERTIES:\n:ID: abc\n:END:\nbody\n"
   "* H1\n:PROPERTIES:\n:ID: abc\n:END:\n\nbody\n"))

(ert-deftest my/org-formatter-test/with-planning-and-drawer ()
  (my/org-formatter-test--run
   "* TODO H1\nSCHEDULED: <2026-05-13>\n:PROPERTIES:\n:ID: abc\n:END:\nbody\n"
   "* TODO H1\nSCHEDULED: <2026-05-13>\n:PROPERTIES:\n:ID: abc\n:END:\n\nbody\n"))

;;; Nested headings

(ert-deftest my/org-formatter-test/nested-headings ()
  (my/org-formatter-test--run
   "* H1\nbody1\n** H2\nbody2\n"
   "* H1\n\nbody1\n\n** H2\n\nbody2\n"))

;;; org-roam title

(ert-deftest my/org-formatter-test/content-with-title ()
  (my/org-formatter-test--run
   "#+title: My Note\n* H1\n"
   "#+title: My Note\n\n* H1\n"))

;;; No org-element cache access
;;
;; `org-at-planning-p' 등 org-element 계열 API를 formatter에서 부르면
;; outline 구조 변경으로 무효화된 org-element-cache를 동기적으로 재파싱해
;; 저장이 느려진다.  formatter는 순수 정규식/버퍼 조작만 써야 한다.
;;
;; 런타임 감시(cl-letf로 org-element-at-point 호출 카운트)는 native-comp가
;; 호출을 직접 링크해 우회하므로 신뢰할 수 없다.  대신 소스에 캐시 접근
;; 심볼이 등장하지 않는지 정적으로 검사한다.

(defconst my/org-formatter-test--cache-symbols
  '("org-element-at-point" "org-element-cache" "org-at-planning-p")
  "formatter가 써서는 안 되는 org-element-cache 접근 심볼.")

;; 로드 시점에 디렉토리를 캡처한다.  테스트 실행 시점에는
;; `load-file-name'/`buffer-file-name'이 nil이라 경로를 못 만든다.
(defconst my/org-formatter-test--dir
  (file-name-directory
   (or load-file-name buffer-file-name (expand-file-name "x")))
  "테스트 파일 디렉토리.  소스 정적 검사에서 formatter 경로를 만든다.")

(ert-deftest my/org-formatter-test/no-org-element-cache-access ()
  (let ((src (expand-file-name "my-org-formatter.el"
                               my/org-formatter-test--dir)))
    (with-temp-buffer
      (insert-file-contents src)
      (dolist (sym my/org-formatter-test--cache-symbols)
        (goto-char (point-min))
        (should-not
         (re-search-forward (concat "\\_<" (regexp-quote sym) "\\_>") nil t))))))

(provide 'my-org-formatter-test)
;;; my-org-formatter-test.el ends here

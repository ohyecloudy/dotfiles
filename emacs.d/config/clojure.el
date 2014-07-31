;;; cider
;;; https://github.com/clojure-emacs/cider
(require 'cider)
(add-hook 'cider-mode-hook 'cider-turn-on-eldoc-mode)
(setq cider-show-error-buffer nil)
(setq cider-repl-result-prefix ";; => ")
(setq cider-interactive-eval-result-prefix ";; => ")
(setq cider-repl-use-clojure-font-lock t)

;;; clojure-mode
;;; https://github.com/technomancy/clojure-mode
(require 'clojure-mode)
;; From 23.2.1, GNU Emacs Lisp Reference Manual edition 3.1:
;; Major modes for editing text should not define <RET> to do anything other than insert a newline.
;; 그래서 RET 바인딩이 사라지고 C-j에 바인딩.
;; 하지만 너무 불편해. 그냥 평소처럼 RET에 바인딩해서 사용한다.
(define-key clojure-mode-map (kbd "RET") 'reindent-then-newline-and-indent)
;; clojurescript
(add-to-list 'auto-mode-alist '("\.cljs$" . clojure-mode))

;;; ac-nrepl
;;; https://github.com/clojure-emacs/ac-nrepl
(require 'ac-nrepl)
(add-hook 'cider-repl-mode-hook 'ac-nrepl-setup)
(add-hook 'cider-mode-hook 'ac-nrepl-setup)
(eval-after-load "auto-complete"
  '(add-to-list 'ac-modes 'cider-repl-mode))

;; tab으로 자동완성
(defun set-auto-complete-as-completion-at-point-function ()
  (setq completion-at-point-functions '(auto-complete)))
(add-hook 'auto-complete-mode-hook 'set-auto-complete-as-completion-at-point-function)
(add-hook 'cider-repl-mode-hook 'set-auto-complete-as-completion-at-point-function)
(add-hook 'cider-mode-hook 'set-auto-complete-as-completion-at-point-function)

;; C-c C-d로 popup doc 띄움
(eval-after-load "cider"
  '(define-key cider-mode-map (kbd "C-c C-d") 'ac-nrepl-popup-doc))


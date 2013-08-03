;; 참고 : http://goo.gl/15KtG (clojure.or.kr)

; theme
(load-file "~/.dotfiles/theme/GNU Emacs/color-theme-tomorrow.el")
(add-to-list 'custom-theme-load-path "~/.dotfiles/theme/GNU Emacs")
(load-theme 'tomorrow-night t)

(setq is-mac (eq system-type 'darwin))

; font
(when is-mac
      (set-face-attribute 'default nil :height 140)
      (set-fontset-font (frame-parameter nil 'font)
			'hangul
			'("Apple SD Gothic Neo" . "ios10646-1")))

; 한글
(set-language-environment "Korean")
(prefer-coding-system 'utf-8)

; startup-message 안 보기
(setq inhibit-startup-message t)
; *scratch* 버퍼 깨끗하게 시작하기
(setq initial-scratch-message nil)
; 컬러 넘버 보기
(setq column-number-mode t)

; 괄호 하이라이팅
(setq show-paren-display 0
     show-paren-style 'expression)
(show-paren-mode t)

; syntax highlighting on
(global-font-lock-mode t)

; tab -> space
(setq indent-tabs-mode nil)

(when (fboundp 'menu-bar-mode) (menu-bar-mode -1))
(when (fboundp 'tool-bar-mode) (tool-bar-mode -1))

; 거슬리는 경고 소리를 끈다.
(setq ring-bell-function 'ignore)

; M-x - C-xC-m
(global-set-key "\C-x\C-m" 'execute-extended-command)

; undo-tree http://www.emacswiki.org/emacs/UndoTree
; evil에서 사용한다.
; 설치해야 Ctrl+R이 redo로 동작
(add-to-list 'load-path "~/.emacs.d/undo-tree")

; evil http://www.emacswiki.org/emacs-en/Evil
(add-to-list 'load-path "~/.emacs.d/evil")
(require 'evil)
(evil-mode 1)
; ctrl+u 를 바인딩.
; (setq evil-want-C-u-scroll t)가 동작 안해서 직접 정의
(define-key evil-motion-state-map (kbd "C-u") 'evil-scroll-up)

;; change mode-line color by evil state
;; http://www.emacswiki.org/emacs/Evil
(lexical-let ((default-color (cons (face-background 'mode-line)
				   (face-foreground 'mode-line))))
  (add-hook 'post-command-hook
	    (lambda ()
	      (let ((color
		     (cond ((minibufferp) default-color)
			   ((evil-insert-state-p) '("#e80000" . "#ffffff"))
			   ((evil-emacs-state-p)  '("#444488" . "#ffffff"))
			   ((buffer-modified-p)   '("#006fa0" . "#ffffff"))
			   (t default-color))))
		(set-face-background 'mode-line (car color))
		(set-face-foreground 'mode-line (cdr color))))))

; clojure-mode https://github.com/technomancy/clojure-mode
(add-to-list 'load-path' "~/.emacs.d/clojure-mode")
(require 'clojure-mode)

; nrepl.el https://github.com/kingtim/nrepl.el
(add-to-list 'load-path' "~/.emacs.d/nrepl.el")
(require 'nrepl)

; markdown-mode http://jblevins.org/projects/markdown-mode/
(add-to-list 'load-path' "~/.emacs.d/markdown-mode")
(autoload 'markdown-mode "markdown-mode" "Major mode for editing Markdown files" t)
(add-to-list 'auto-mode-alist '("\\.text\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))

; ac-nrepl https://github.com/clojure-emacs/ac-nrepl
(require 'ac-nrepl)
(add-hook 'nrepl-mode-hook 'ac-nrepl-setup)
(add-hook 'nrepl-interaction-mode-hook 'ac-nrepl-setup)
(eval-after-load "auto-complete"
  '(add-to-list 'ac-modes 'nrepl-mode))

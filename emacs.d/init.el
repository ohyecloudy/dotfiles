(setq windows? (eq system-type 'windows-nt))
(setq mac? (eq system-type 'darwin))

(when mac?
  ;; font
  (set-face-attribute 'default nil :height 150)
  (set-fontset-font (frame-parameter nil 'font)
		    'hangul
		    '("Apple SD Gothic Neo" . "ios10646-1"))
  (setq-default line-spacing 1))

(defun available-font? (font) (member font (font-family-list)))
(when windows?
  ;; font
  (when (available-font? "DejaVu Sans Mono")
    (set-face-attribute 'default nil
			:font "Dejavu Sans Mono-11"
			:weight `normal)
    (setq-default line-spacing 3)))

;; 한글
(set-language-environment "Korean")
;; 한글 환경에서는 cp949 인코딩이 디폴트이기 때문.
(prefer-coding-system 'utf-8)

;; startup-message 안 보기
(setq inhibit-startup-message t)
;; *scratch* 버퍼 깨끗하게 시작하기
(setq initial-scratch-message nil)
;; 선택 텍스트를 타이핑할 때, 삭제
(delete-selection-mode t)
;; 라인 넘버 보기
(global-linum-mode t) 
;; 컬럼 넘버 보기
(setq column-number-mode t)
;; word-wrap
(global-visual-line-mode t)
;; 커서가 있는 라인 하이라이트
(global-hl-line-mode t)

;; syntax highlighting on
(global-font-lock-mode t)

;; tab -> space
(setq indent-tabs-mode nil)

;; find-file, switch-to-buffer에서 file 이름을 보여주는 mode
(ido-mode t)

(when (fboundp 'menu-bar-mode) (menu-bar-mode t))
(when (fboundp 'tool-bar-mode) (tool-bar-mode -1))

;; 거슬리는 경고 소리를 끈다.
(setq ring-bell-function 'ignore)

;; 마우스 꺼져. 타이핑을 시작하면 구석으로 마우스 커서를 치운다.
(mouse-avoidance-mode 'banish)

;; M-x - C-xC-m
(global-set-key "\C-x\C-m" 'execute-extended-command)

;;; packages
(require 'package)
(add-to-list 'package-archives
	     '("melpa" . "http://melpa.milkbox.net/packages/")
	     t)
(package-initialize)

(defvar ohyecloudy/packages '(clojure-mode
                              cider
                              undo-tree
                              evil
			      auto-complete
                              ac-nrepl
			      highlight-parentheses
			      edit-server
			      color-theme-solarized
			      markdown-mode
			      magit))

(dolist (pkg ohyecloudy/packages)
  (when (not (package-installed-p pkg))
    (package-install pkg)))

;;; clojure-mode https://github.com/technomancy/clojure-mode
(require 'clojure-mode)
;; From 23.2.1, GNU Emacs Lisp Reference Manual edition 3.1:
;; Major modes for editing text should not define <RET> to do anything other than insert a newline.
;; 그래서 RET 바인딩이 사라지고 C-j에 바인딩.
;; 하지만 너무 불편해. 그냥 평소처럼 RET에 바인딩해서 사용한다.
(define-key clojure-mode-map (kbd "RET") 'reindent-then-newline-and-indent)
;; clojurescript
(add-to-list 'auto-mode-alist '("\.cljs$" . clojure-mode))

;;; cider https://github.com/clojure-emacs/cider
(require 'cider)

;;; undo-tree http://www.emacswiki.org/emacs/UndoTree
;; evil에서 사용한다.
;; 설치해야 Ctrl+R이 redo로 동작

;;; evil http://www.emacswiki.org/emacs-en/Evil
(require 'evil)
(evil-mode 1)

;; ctrl+u 를 바인딩.
;; (setq evil-want-C-u-scroll t)가 동작 안해서 직접 정의
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

;;; auto-complete https://github.com/auto-complete/auto-complete
(require 'auto-complete-config)
(ac-config-default)

;;; ac-nrepl https://github.com/clojure-emacs/ac-nrepl
(require 'ac-nrepl)
(add-hook 'nrepl-mode-hook 'ac-nrepl-setup)
(add-hook 'nrepl-interaction-mode-hook 'ac-nrepl-setup)
(eval-after-load "auto-complete"
  '(add-to-list 'ac-modes 'nrepl-mode))

;;; highlight-parentheses https://github.com/nschum/highlight-parentheses.el
(require 'highlight-parentheses)
(setq hl-paren-colors nil)
(setq hl-paren-background-colors '("gray"))
;; global로 highlight-parentheses minor mode를 활성화 http://goo.gl/ig5YuY 참고
(define-global-minor-mode global-highlight-parentheses-minor-mode
  highlight-parentheses-mode highlight-parentheses-mode)
(global-highlight-parentheses-minor-mode t)

(setq show-paren-display 0)
(show-paren-mode t)

;;; edit-server http://www.emacswiki.org/emacs/Edit_with_Emacs
(require 'edit-server)
(edit-server-start)
(setq edit-server-new-frame nil)

;;; solarized theme https://github.com/sellout/emacs-color-theme-solarized
(load-theme 'solarized-light t)

;;; markdown-mode http://jblevins.org/projects/markdown-mode/
(autoload 'markdown-mode "markdown-mode"
  "Major mode for editing Markdown files" t)
(add-to-list 'auto-mode-alist '("\\.text\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))

;;; emacs-lisp-mode
(add-hook 'emacs-lisp-mode-hook
	  (lambda ()
	    ;; clojure cider mode에서 쓰는 키와 맞춘다. C-M-x는 입력이 괴로움
	    (define-key emacs-lisp-mode-map "\C-c\C-c" 'eval-defun)))

;;; lisp-interaction-mode
(add-hook 'lisp-interaction-mode-hook
	  (lambda ()
	    ;; clojure cider mode에서 쓰는 키와 맞춘다. C-M-x는 입력이 괴로움
	    (define-key lisp-interaction-mode-map "\C-c\C-c" 'eval-defun)))

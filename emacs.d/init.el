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
;; 날짜 표시를 영어로하려고
(setq system-time-locale "C")

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

(global-auto-revert-mode 1)

;; tab -> space
(setq indent-tabs-mode nil)

;; find-file, switch-to-buffer에서 file 이름을 보여주는 mode
(ido-mode t)

(when (fboundp 'menu-bar-mode) (menu-bar-mode -1))
(when (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(when (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))

;; 거슬리는 경고 소리를 끈다.
(setq ring-bell-function 'ignore)

(defalias 'yes-or-no-p 'y-or-n-p)
(defalias 'sh 'shell)

;; M-x - C-xC-m
(global-set-key "\C-x\C-m" 'execute-extended-command)

;;; packages
(require 'package)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/")
             t)
(package-initialize)
(when (not (package-installed-p 'use-package))
  (package-refresh-contents)
  (package-install 'use-package))

;;; evil
;;; https://www.emacswiki.org/emacs/Evil
(use-package evil
  :ensure t
  :init
  (setq evil-want-C-u-scroll t)
  :config
  (evil-mode t)
  (evil-set-initial-state 'calendar-mode 'emacs)
  (evil-set-initial-state 'calculator-mode 'emacs)
  (evil-set-initial-state 'git-rebase-mode 'emacs)
  (evil-set-initial-state 'magit-blame-mode 'emacs)
  (setq-default evil-symbol-word-search t))

;;; evil-matchit
;;; https://github.com/redguardtoo/evil-matchit
(use-package evil-matchit
  :ensure t
  :config
  (global-evil-matchit-mode t))

;;; https://github.com/bbatsov/solarized-emacs
(use-package solarized-theme
  :ensure t
  :config
  (load-theme 'solarized-light 'NO-CONFIRM))

(defvar ohyecloudy/packages '(markdown-mode
                              yaml-mode
                              smart-mode-line
                              org-journal
                              elpy
                              coffee-mode
                              ranger
                              magit))

(dolist (pkg ohyecloudy/packages)
  (when (not (package-installed-p pkg))
    (package-install pkg)))

(add-to-list 'load-path "~/.emacs.d/config")
(load "my-platform.el")
(load "my-backup.el")
(load "my-ws.el")
(load "my-org.el")
(load "my-python.el")

(setq show-paren-display 0)
(show-paren-mode t)

;;; emacs-server
(require 'server)
(server-start)

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

;;; shell
(when windows?
  (let* ((git-dir "C:/Program Files/Git")
         (bash-dir (concat (file-name-as-directory git-dir) "bin")))
    (setq explicit-shell-file-name (concat (file-name-as-directory bash-dir)
                                           "bash.exe"))
    (setq shell-file-name explicit-shell-file-name)
    (add-to-list 'exec-path git-dir)
    (add-to-list 'exec-path bash-dir)
    (setq explicit-bash.exe-args '("--noediting" "--login" "-i"))
    (setenv "SHELL" shell-file-name)
    (setenv "PATH" (concat git-dir path-separator
                           (concat bash-dir path-separator (getenv "PATH"))))))

;; shell mode hook
(add-hook 'shell-mode-hook
          (lambda ()
            ;; evil-scroll-up과 충돌
            (define-key shell-mode-map "\C-d" nil)))

;;; http://robots.thoughtbot.com/no-newline-at-end-of-file
(setq require-final-newline t)

;;; title bar
(setq frame-title-format "%b")

;;; smart-mode-line
(setq sml/no-confirm-load-theme t)
(setq sml/show-eol t) ;; show end-of-line. ex) CRLF(dos)
(setq sml/theme 'respectful)
(sml/setup)
(add-to-list 'sml/replacer-regexp-list '("^c:/work/" ":Dev:") t)

;;; prettify-symbols-mode
(add-hook 'prog-mode-hook 'prettify-symbols-mode)

;;; ibuffer-mode
(defalias 'list-buffers 'ibuffer)
(setq ibuffer-expert t)
(setq ibuffer-default-sorting-mode 'major-mode)
(add-hook 'ibuffer-mode-hook
          '(lambda ()
             (ibuffer-auto-mode 1)
             (add-to-list 'ibuffer-never-show-predicates "^\\*")))

;;; grep
(setq grep-command "grep -nH -i -r ")

;;; ranger
(ranger-override-dired-mode t)

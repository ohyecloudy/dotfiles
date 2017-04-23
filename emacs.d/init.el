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

(when (fboundp 'menu-bar-mode) (menu-bar-mode -1))
(when (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(when (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))

;; 거슬리는 경고 소리를 끈다.
(setq ring-bell-function 'ignore)

(defalias 'yes-or-no-p 'y-or-n-p)
(defalias 'sh 'shell)

;;; packages
(require 'package)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/")
             t)
(add-to-list 'package-archives
             '("org" . "http://orgmode.org/elpa/")
             t)
(package-initialize)
(when (not (package-installed-p 'use-package))
  (package-refresh-contents)
  (package-install 'use-package))

;;; evil
;;; https://www.emacswiki.org/emacs/Evil
(setq evil-want-C-u-scroll t) ;; :init에 넣어도 동작 안 함 Evil version 1.2.12
(use-package evil
  :ensure t
  :config
  (evil-mode t)
  (evil-set-initial-state 'calendar-mode 'emacs)
  (evil-set-initial-state 'calculator-mode 'emacs)
  (evil-set-initial-state 'git-rebase-mode 'emacs)
  (evil-set-initial-state 'finder-mode 'emacs)
  (evil-set-initial-state 'Man-mode 'emacs)
  (setq-default evil-symbol-word-search t))

;;; evil-matchit
;;; https://github.com/redguardtoo/evil-matchit
(use-package evil-matchit
  :ensure t
  :config
  (global-evil-matchit-mode t))

;;; evil-visualstar
;;; https://github.com/bling/evil-visualstar
(use-package evil-visualstar
  :ensure t
  :config
  (global-evil-visualstar-mode t))

;;; https://github.com/bbatsov/solarized-emacs
(use-package solarized-theme
  :ensure t
  :config
  (load-theme 'solarized-light 'NO-CONFIRM))

;;; http://jblevins.org/projects/markdown-mode/
(use-package markdown-mode
  :ensure t
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :init (setq markdown-command "multimarkdown"))

;;; Elpy, the Emacs Lisp Python Environment
;;; elpy https://github.com/jorgenschaefer/elpy
(use-package elpy
  :ensure t
  :config (elpy-enable))

;;; https://github.com/Malabarba/smart-mode-line
(use-package smart-mode-line
  :ensure t
  :config
  (setq sml/no-confirm-load-theme t)
  (setq sml/show-eol t) ;; show end-of-line. ex) CRLF(dos)
  (setq sml/theme 'respectful)
  (sml/setup)
  (add-to-list 'sml/replacer-regexp-list '("^c:/work/" ":Dev:") t))

;;; https://github.com/ralesi/ranger.el
(use-package ranger
  :ensure t
  :config (ranger-override-dired-mode t))

;;; https://github.com/yoshiki/yaml-mode
(use-package yaml-mode :ensure t)

;;; https://github.com/magit/magit
(use-package magit
  :ensure t
  :bind ("C-x g" . magit-status)
  :config
  ;; (evil-set-initial-state 'magit-blame-mode 'emacs) 동작을 안 해서
  ;; 원인은 모름
  ;; Magit 20170322.1550, Git 2.11.0.windows.1, Emacs 24.4.1, windows-nt
  ;; Evil version 1.2.12
  (add-hook 'magit-blame-mode-hook
            (lambda ()
              (if magit-blame-mode
                  (evil-emacs-state 1)
                (evil-normal-state 1)))))

;;; https://github.com/emacs-helm/helm
(use-package helm
  :ensure t
  :diminish helm-mode
  :bind (("M-x" . helm-M-x)
         ("C-x C-m" . helm-M-x)
         ("C-x b" . helm-mini)
         ("C-x C-f" . helm-find-files))
  :init
  (setq helm-split-window-in-side-p t)
  :config
  (helm-mode 1)
  (helm-autoresize-mode 1))

;;; http://company-mode.github.io/
(use-package company
  :ensure t
  :init
  (add-hook 'after-init-hook 'global-company-mode)
  :config
  (setq company-idle-delay 0)
  (setq company-show-numbers "on"))

;;; org-mode에서 #+ 다음에 completion cadidates가 나오도록
;;; https://emacs.stackexchange.com/a/30691
(defun org-keyword-backend (command &optional arg &rest ignored)
  (interactive (list 'interactive))
  (cl-case command
    (interactive (company-begin-backend 'org-keyword-backend))
    (prefix (and (eq major-mode 'org-mode)
                 (cons (company-grab-line "^#\\+\\(\\w*\\)" 1)
                       t)))
    (candidates (mapcar #'upcase
                        (cl-remove-if-not
                         (lambda (c) (string-prefix-p arg c))
                         (pcomplete-completions))))
    (ignore-case t)
    (duplicates t)))
(add-to-list 'company-backends 'org-keyword-backend)

;;; https://github.com/manuel-uberti/helm-company
(use-package helm-company
  :ensure t
  :config
  (eval-after-load 'company
    '(progn
       (define-key company-mode-map (kbd "C-:") 'helm-company)
       (define-key company-active-map (kbd "C-:") 'helm-company))))

(setq show-paren-display 0)
(show-paren-mode t)

;;; emacs-server
(require 'server)
(server-start)

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
  (let* ((combine-path (lambda (dir dir-or-file)
                         (concat (file-name-as-directory dir) dir-or-file)))
         (base-dir "C:/git-sdk-64")
         (mingw64-bin-dir (funcall combine-path base-dir "mingw64/bin"))
         (msys2-bin-dir (funcall combine-path base-dir "usr/bin"))
         (bash-path (funcall combine-path msys2-bin-dir "bash.exe")))
    (add-to-list 'exec-path msys2-bin-dir)
    (add-to-list 'exec-path mingw64-bin-dir)
    (setq explicit-shell-file-name bash-path)
    (setq shell-file-name bash-path)
    (setenv "SHELL" bash-path)
    (setq explicit-bash.exe-args '("--noediting" "--login" "-i"))
    (setenv "PATH" (concat mingw64-bin-dir path-separator
                           (concat msys2-bin-dir path-separator
                                   (getenv "PATH"))))))

;; shell mode hook
(add-hook 'shell-mode-hook
          (lambda ()
            ;; evil-scroll-up과 충돌
            (define-key shell-mode-map "\C-d" nil)))

;;; http://robots.thoughtbot.com/no-newline-at-end-of-file
(setq require-final-newline t)

;;; title bar
(setq frame-title-format "%b")

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
;; grep: warning: GREP_OPTIONS is deprecated; please use an alias or script
;; grep-highlight-matches 변수를 세팅하니 경고 메시지 작렬
;; 그래서 고쳐지기 전까지는 --color 옵션을 직접 세팅해준다.
(setq grep-template "grep <X> --color=always <C> -nH <R> <F>")
(setq grep-find-template
      "find . <X> -type f <F> -exec grep --color=always <C> -nH <R> {} \\;")

;;; PATH env
(setq mac? (eq system-type 'darwin))
(when mac?
  (let ((usr-local "/usr/local/bin"))
    (add-to-list 'exec-path usr-local)
    (setenv "PATH" (concat usr-local path-separator (getenv "PATH")))))

;;; backup
(add-to-list 'backup-directory-alist '("." . "~/.emacs-saves"))
(setq delete-old-versions t
      kept-old-versions 2
      kept-new-versions 2
      version-control t)

;; whitespace mode
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(whitespace-line ((nil (:bold t :background "yellow"))))
 '(whitespace-tab ((nil (:bold t :background "linen"))))
 '(whitespace-trailing ((nil (:bold t :background "red1")))))

(global-whitespace-mode t)

(add-hook
 'after-change-major-mode-hook
 '(lambda ()
    (if (derived-mode-p 'prog-mode)
        (setq whitespace-line-column 80
              whitespace-style '(face trailing lines-tail tab-mark))
      (setq whitespace-line-column nil
            whitespace-style '(face trailing tab-mark)))))

;; disable tabs mode
(setq-default indent-tabs-mode nil)

(add-hook 'before-save-hook 'delete-trailing-whitespace)

(use-package org
  :pin org
  :ensure org-plus-contrib
  :config
  (require 'ox-confluence)
  (setq org-startup-with-inline-images t)
  ;; org keyword를 company 모드 completion에 추가함
  ;; https://emacs.stackexchange.com/a/21173
  (add-hook 'org-mode-hook
          (lambda ()
            (add-hook 'completion-at-point-functions
                      'pcomplete-completions-at-point
                      nil
                      t)))
  ;; Make RET also indent
  ;; https://github.com/pkkm/.emacs.d/blob/master/conf/mode-specific/org.el
  (bind-key [remap org-return] #'org-return-indent org-mode-map)

  (org-babel-do-load-languages
   'org-babel-load-languages
   '((emacs-lisp . t)
     (plantuml . t)
     (python . t)
     (C . t)))

  ;; plantuml
  (setq org-confirm-babel-evaluate nil)
  (setq org-plantuml-jar-path
        (expand-file-name "~/bin/plantuml.jar"))
  (add-hook 'org-babel-after-execute-hook
            (lambda ()
              (when org-inline-image-overlays
                (org-redisplay-inline-images))))
  (add-to-list 'org-structure-template-alist
               '("u" "#+BEGIN_SRC plantuml :file ?.png\nskinparam monochrome true\n#+END_SRC")))

;;; http://emacsredux.com/blog/2013/03/27/copy-filename-to-the-clipboard/
(defun copy-file-name-to-clipboard ()
  "Copy the current buffer file name to the clipboard."
  (interactive)
  (let ((filename (if (equal major-mode 'ranger-mode)
                      default-directory
                    (buffer-file-name))))
    (when filename
      (kill-new filename)
      (message "Copied buffer file name '%s' to the clipboard." filename))))
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (org-plus-contrib helm magit yaml-mode ranger smart-mode-line elpy markdown-mode solarized-theme evil-visualstar evil-matchit evil use-package))))

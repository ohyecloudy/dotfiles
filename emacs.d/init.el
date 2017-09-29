(setq windows? (eq system-type 'windows-nt))
(setq mac? (eq system-type 'darwin))

(when mac?
  ;; font
  (set-face-attribute 'default nil :height 150)
  (set-fontset-font (frame-parameter nil 'font)
                    'hangul
                    '("Apple SD Gothic Neo" . "ios10646-1"))
  (setq-default line-spacing 1)

  ;; keybinding
  (setq mac-command-modifier 'meta)
  (setq mac-option-modifier 'super))

(defun available-font? (font) (member font (font-family-list)))

;; | 12345678 |   |
;; |----------+---|
;; | 일이삼사 |   |
(when windows?
  (when (available-font? "Consolas")
    (set-frame-font "Consolas-13" nil t)
    (set-fontset-font t 'hangul (font-spec :name "Batangche"))
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
  (evil-set-initial-state 'helm-ag-mode 'emacs)
  (setq-default evil-symbol-word-search t)

  ;; http://blog.binchen.org/posts/auto-complete-word-in-emacs-mini-buffer-when-using-evil.html
  ;; / 문자를 Punctuation characters로 변경함
  ;; %s/old/new/g 입력할 때, M-/ 누르면 자동완성 된다.
  (defun minibuffer-inactive-mode-hook-setup ()
    ;; make `try-expand-dabbrev' from `hippie-expand' work in mini-buffer
    ;; @see `he-dabbrev-beg', so we need re-define syntax for '/'
    (set-syntax-table (let* ((table (make-syntax-table)))
                        (modify-syntax-entry ?/ "." table)
                        table)))
  (add-hook 'minibuffer-inactive-mode-hook 'minibuffer-inactive-mode-hook-setup))

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
  :init
  ;; org에서 커진 한글 폰트가 너무 안 예뻐서
  (setq solarized-height-minus-1 1.0)
  (setq solarized-height-plus-1 1.0)
  (setq solarized-height-plus-2 1.0)
  (setq solarized-height-plus-3 1.0)
  (setq solarized-height-plus-4 1.0)
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
  (add-to-list 'sml/replacer-regexp-list '("^c:/work/" ":Dev:") t)
  (add-to-list 'rm-blacklist " WS" t)
  (add-to-list 'rm-blacklist " Undo-Tree" t)
  (add-to-list 'rm-blacklist " Wrap" t))

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
  (helm-autoresize-mode 1)

  (when windows?
    ;; helm-ag 명령어 실행시 hang
    ;; 해결은 아니라 delay를 줘서 회피하는 반창고
    ;; https://github.com/syohex/emacs-helm-ag/issues/188
    (setq helm-input-idle-delay 0.1)
    (setq helm-cycle-resume-delay 2)
    (setq helm-follow-input-idle-delay 1)))

;;; https://github.com/syohex/emacs-helm-ag
(use-package helm-ag
  :ensure t
  :config

  ;; windows에서만 문제가 발생
  (when windows?
    ;; mingw64/mingw-w64-x86_64-ag 2.0.0.r1912.ccdbe93-1 사용시
    ;; 한글 검색이 안 된다. grep, rip 모두 잘 되는 걸로 봐서는 패키지를 의심
    ;; helm-ag 패키지로도 사용할 수 있는 ripgrep을 사용한다.
    ;; https://github.com/BurntSushi/ripgrep
    ;; macOS에서도 ag 대신 ripgrep을 사용할지는 고민 중.
    (setq helm-ag-base-command "rg --no-heading --vimgrep")

    ;; helm-do-ag 처럼 process로 한글 인자를 넘길 때, encoding 문제를 해결하기 위해
    ;; 내부 동작을 정확히 파악하지 못했다.
    ;;
    ;; cp949일 때
    ;; - (korean-iso-8bit-dos . korean-iso-8bit-unix)
    ;; - 출력은 깨지지만 입력은 process로 제대로 전달된다.
    ;; utf-8일 때
    ;; - (utf-8-dos . utf-8-unix)
    ;; - 입력은 깨지지만 출력은 제대로 된다.
    ;;
    ;; 둘을 조합했다.
    ;; 다른 건 utf-8로 잘 동작하니 helm-do-ag 실행할 때만 프로세스 인코딩을 변경한다
    (advice-add 'helm-do-ag
                :before (lambda (&rest _)
                          (setq default-process-coding-system
                                '(utf-8-dos . korean-iso-8bit-unix))))
    (advice-add 'helm-do-ag
                :after (lambda (&rest _)
                         (setq default-process-coding-system
                               '(utf-8-dos . utf-8-unix))))))

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

;;; https://github.com/Kitware/CMake
(use-package cmake-mode :ensure t)

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
    (setq whitespace-line-column nil
          whitespace-style '(face trailing))))

;; disable tabs mode
(setq-default indent-tabs-mode nil)

(add-hook 'before-save-hook 'delete-trailing-whitespace)

;;; compose-mail 바인딩 키 제거
(global-set-key (kbd "C-x m") nil)

;;; https://github.com/zweifisch/ob-http
(use-package ob-http :ensure t)

(use-package org
  :pin org
  :ensure org-plus-contrib
  :bind (("C-c a" . org-agenda)
         ("C-c c" . org-capture))
  :config
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

  (setq org-todo-keywords
        '((sequence "TODO" "STARTED" "|" "DONE")))

  (setq org-todo-keyword-faces
        '(("STARTED" . (:background "yellow" :weight bold))))

  ;; http://sachachua.com/blog/2007/12/clocking-time-with-emacs-org/
  ;; STARTED 키워드로 변경되면 org-clock 시작
  ;; 또는 org-clock을 시작하면 STARTED 키워드로 변경
  (progn
    (defun wicked/org-clock-in-if-starting ()
      "Clock in when the task is marked STARTED."
      (when (and (string= org-state "STARTED")
                 (not (string= org-last-state org-state)))
        (org-clock-in)))
    (add-hook 'org-after-todo-state-change-hook
              'wicked/org-clock-in-if-starting)
    (defadvice org-clock-in (after wicked activate)
      "Set this task's status to 'STARTED'."
      (org-todo "STARTED")))

  ;; org-clock persistence 설정. 컴퓨터 꺼도 emacs 시계는 굴러간다.
  ;; https://writequit.org/denver-emacs/presentations/2017-04-11-time-clocking-with-org.html
  (progn
    (org-clock-persistence-insinuate)
    (setq org-clock-persist t)
    (setq org-clock-in-resume t)
    (setq org-clock-persist-query-resume nil))

  ;; 자리비움 감지 기준
  (setq org-clock-idle-time 15)

  ;; org-set-effort 함수 실행 시 나오는 preset 리스트
  (setq org-global-properties
        '(("Effort_ALL" .
           "1:00 2:00 3:00 4:00 8:00 16:00 24:00 32:00 40:00 0:30")))
  ;; hotkey 1    2    3    4    5    6     7     8     9     0

  ;; org-columns에서 effort를 볼 수 있게 추가
  (setq org-columns-default-format "%50ITEM(Task) %10Effort{:} %10CLOCKSUM")

  ;; org-clock-report 기본 프로퍼티
  (setq org-clock-clocktable-default-properties
        '(:maxlevel 2 :scope file :properties ("effort")))

  (setq org-tag-alist '((:startgroup . nil)
                        ("task" . ?t)
                        ("codereview" . ?c)
                        ("meeting" . ?m)
                        ("interview" . ?i)
                        ("wiki" . ?w)
                        ("til" . ?l)
                        (:endgroup . nil)
                        (:startgroup . nil)
                        ("blog" . ?b)
                        ("trivial" . ?t)
                        ("sideproject" . ?s)
                        (:endgroup . nil)))

  ;; agenda
  (setq org-agenda-files '("~/org"
                           "~/org/work"
                           "~/org/side"))

  ;; capture
  (setq org-capture-templates
        '(("j"
           "Journal"
           entry
           (file+datetree+prompt (format-time-string "~/journal/%B.org"))
           "* %?")))

  (org-babel-do-load-languages
   'org-babel-load-languages
   '((emacs-lisp . t)
     (plantuml . t)
     (python . t)
     (http . t)
     (C . t)))

  ;; <h가 #+BEGIN_HTML #+END_HTML에서 #+BEGIN_EXPORT html #+END_EXPORT로 변경됨
  ;; jekyll-org가 지원 안해서 예전으로 되돌림
  (add-to-list 'org-structure-template-alist
               '("h" "#+BEGIN_HTML\n?\n#+END_HTML"))

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

;;; https://github.com/krisajenkins/ob-translate
(use-package ob-translate
  :ensure t
  :config
  (setq ob-translate:default-dest "ko"))

;;; https://github.com/larstvei/ox-gfm
(use-package ox-gfm :ensure t)

;;; https://github.com/clojure-emacs/clojure-mode
(use-package clojure-mode :ensure t)

;;; https://github.com/elixir-editors/emacs-elixir
(use-package elixir-mode :ensure t)

;;; cc-mode
(setq-default c-default-style "bsd"
              c-basic-offset 4)

;;; local package
(add-to-list 'load-path "~/.emacs.d/lisp/")
(require 'my-ox-confluence)

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
    (ob-translate org-plus-contrib ob-http helm-company helm magit yaml-mode ranger smart-mode-line elpy markdown-mode solarized-theme evil-visualstar evil-matchit evil use-package))))

(defconst local-init-el-path "~/.emacs.d/init.el.local")
(when (file-exists-p local-init-el-path)
  (message (format "load local init el - %s" local-init-el-path))
  (load-file local-init-el-path))

(setq windows? (eq system-type 'windows-nt))
(setq mac? (eq system-type 'darwin))

(defun available-font? (font) (member font (font-family-list)))

;;
;; macbook pro에서 full height에 스크린 width 반정도 차지하게 세팅
;;
(when mac?
  (setq default-frame-alist '((left . 0) (width . 100) (fullscreen . fullheight))))

;; | 12345678 |   |
;; |----------+---|
;; | 일이삼사 |   |
(when mac?
  ;; font
  (when (available-font? "Consolas")
    (set-frame-font "Consolas-15" nil t)
    (set-fontset-font t 'hangul (font-spec :name "PCMyungjo-16"))
    (setq-default line-spacing 2))

  ;; keybinding
  (setq mac-command-modifier 'meta)
  (setq mac-option-modifier 'super))

;; | 12345678 |   |
;; |----------+---|
;; | 일이삼사 |   |
(when windows?
  (when (available-font? "Consolas")
    (set-frame-font "Consolas-11" nil t)
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
;; word-wrap
(global-visual-line-mode t)
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

;;; emacs가 init.el에 추가하는 설정 방지
;;; (custom-set-variables ...
;;; https://jamiecollinson.com/blog/my-emacs-config/
(setq custom-file (make-temp-file "emacs-custom"))

;;; packages
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/") t)
(package-initialize)
(when (not (package-installed-p 'use-package))
  (package-refresh-contents)
  (package-install 'use-package))

;;;
;;; doom-themes
;;; - https://github.com/hlissner/emacs-doom-themes
;;;
(use-package doom-themes
  :ensure t
  :init
  (setq doom-themes-enable-bold nil
        doom-themes-enable-italic nil)
  :config
  (load-theme 'doom-one t)
  ;; Enable flashing mode-line on errors
  (doom-themes-visual-bell-config)
  ;; Corrects (and improves) org-mode's native fontification.
  (doom-themes-org-config))

;; 커서가 있는 라인 하이라이트
(global-hl-line-mode t)

;;; flymake
(global-set-key (kbd "C-S-p") 'flymake-goto-prev-error)
(global-set-key (kbd "C-S-n") 'flymake-goto-next-error)

;;; evil
;;; https://www.emacswiki.org/emacs/Evil
(setq evil-want-C-u-scroll t) ;; :init에 넣어도 동작 안 함 Evil version 1.2.12
(setq evil-want-C-w-in-emacs-state t) ;; :init에 넣어도 동작 안 함
(use-package evil
  :ensure t
  ;; 쓰지 않는 키바인딩. alchemist-mode에서 사용하려고 unbinding
  :bind (:map
         evil-normal-state-map
         ("M-." . nil) ("M-," . nil))
  :init
  (setq evil-want-C-w-delete nil)
  :config
  (evil-mode t)
  (evil-set-initial-state 'calendar-mode 'emacs)
  (evil-set-initial-state 'calculator-mode 'emacs)
  (evil-set-initial-state 'git-rebase-mode 'emacs)
  (evil-set-initial-state 'finder-mode 'emacs)
  (evil-set-initial-state 'Man-mode 'emacs)
  (evil-set-initial-state 'helm-ag-mode 'emacs)
  (evil-set-initial-state 'ert-results-mode 'emacs)
  (evil-set-initial-state 'ert-simple-view-mode 'emacs)
  (evil-set-initial-state 'process-menu-mode 'emacs)
  (evil-set-initial-state 'alchemist-mix-mode 'emacs)
  (evil-set-initial-state 'alchemist-hex-mode 'emacs)
  (evil-set-initial-state 'alchemist-help-minor-mode 'emacs)
  (evil-set-initial-state 'alchemist-test-report-mode 'emacs)
  (evil-set-initial-state 'xref--xref-buffer-mode 'emacs)
  (setq-default evil-symbol-word-search t)

  (add-hook 'emacs-lisp-mode-hook
            #'(lambda ()
                (modify-syntax-entry ?_ "w")
                (modify-syntax-entry ?- "w")))
  (add-hook 'c-mode-common-hook
            #'(lambda () (modify-syntax-entry ?_ "w")))
  (add-hook 'elixir-mode-hook
            #'(lambda ()
                (modify-syntax-entry ?_ "w")
                (modify-syntax-entry ?: ".")
                (modify-syntax-entry ?% ".")))

  ;; Http://blog.binchen.org/posts/auto-complete-word-in-emacs-mini-buffer-when-using-evil.html
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

;;;
;;; doom-modeline
;;; - https://github.com/seagle0128/doom-modeline
;;; - 폰트가 이상하게 나오면 M-x all-the-icons-install-fonts 실행
;;;
(use-package doom-modeline
  :ensure t
  :hook (after-init . doom-modeline-mode))

;;; https://github.com/purcell/exec-path-from-shell
;;; emacs를 GUI로 실행했을 때, shell의 PATH 환경 변수가 적용 안 되는 문제를 해결하려고
(when mac?
  (use-package exec-path-from-shell :ensure t)
  (exec-path-from-shell-initialize))

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
                (evil-normal-state 1))))
  ;; commit message 편집하는 버퍼가 열리면 evil-emacs-state로 켜짐
  ;; Magit 20171031.1141, Git 2.14.1.windows.1, Emacs 25.2.1, windows-nt
  (add-hook 'git-commit-setup-hook (lambda () (evil-normal-state 1))))

;;; https://github.com/emacs-helm/helm
(use-package helm
  :ensure t
  :diminish helm-mode
  :bind (("M-x" . helm-M-x)
         ("C-x C-m" . helm-M-x)
         ("C-x b" . helm-mini)
         ("C-x C-f" . helm-find-files))
  :config
  (require 'helm-config)
  ;; http://tuhdo.github.io/helm-intro.html 권고에 따라 키 바꿈 C-x C-c 실수에 동의
  (progn
    (global-set-key (kbd "C-c h") 'helm-command-prefix)
    (global-unset-key (kbd "C-x c")))
  (progn
    (global-set-key (kbd "C-c h s") 'helm-do-ag)
    (global-set-key (kbd "C-c h o") 'helm-occur))

  (setq helm-split-window-inside-p t
        helm-M-x-fuzzy-match t
        helm-buffers-fuzzy-matching t
        helm-recentf-fuzzy-match t
        helm-apropos-fuzzy-match t)
  (helm-autoresize-mode 1)

  (helm-mode 1))

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
    (setq helm-ag-base-command "rg -i --no-heading --vimgrep")

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

;;; https://github.com/ShingoFukuyama/helm-swoop
(use-package helm-swoop
  :ensure t
  :bind
  (("M-i" . helm-swoop)
   ("M-I" . helm-swoop-back-to-last-point)
   ("C-c M-i" . helm-multi-swoop)
   ("C-x M-i" . helm-multi-swoop-all)
   :map
   helm-swoop-map
   ("M-i" . helm-multi-swoop-all-from-helm-swoop)
   ("M-m" . helm-multi-swoop-current-mode-from-helm-swoop)))

;;; https://github.com/bbatsov/projectile
(use-package projectile
  :ensure t
  :init
  (setq projectile-keymap-prefix (kbd "C-c p"))
  :config
  (projectile-mode)
  (setq projectile-enable-caching t)
  (setq projectile-indexing-method 'alien)
  (setq projectile-globally-ignored-file-suffixes
        '(".psd" ".png" ".fbx" ".anim" ".mat" ".meta" ".prefab" ".asset"
          ".controller")))

;;; https://github.com/bbatsov/helm-projectile
(use-package helm-projectile
  :ensure t
  :config
  (helm-projectile-on)

  ;; windows에서는 ag대신 ripgrep을 사용.
  ;; --ignore 옵션이 하드코딩돼서 ripgrep을 사용 못함
  ;; projectile 0.14.0
  (when windows?
    (advice-add 'helm-do-ag
                :before (lambda (&rest _)
                          (setq helm-ag-base-command
                                (replace-regexp-in-string
                                 "--ignore.*"
                                 ""
                                 helm-ag-base-command))))))

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

;;; https://github.com/dimitri/el-get
(progn
  (add-to-list 'load-path "~/.emacs.d/el-get/el-get")
  (unless (require 'el-get nil 'noerror)
    (package-install 'el-get)
    (require 'el-get))

  (add-to-list 'el-get-recipe-path "~/.emacs.d/el-get-user/recipes"))

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

;;; https://github.com/AndreaCrotti/yasnippet-snippets
(use-package yasnippet-snippets :ensure t)

;;; https://github.com/joaotavora/yasnippet
(use-package yasnippet
  :ensure t
  :config
  (yas-global-mode 1))

;;; https://github.com/pashky/restclient.el
(use-package restclient :ensure t)

;;; https://github.com/alf/ob-restclient.el
(use-package ob-restclient :ensure t)

;;; https://github.com/zweifisch/ob-elixir
(use-package ob-elixir :ensure t)

(use-package org
  :pin org
  :ensure org-plus-contrib
  :defer t
  :bind (("C-c c" . org-capture)
         :map
         org-mode-map
         ("C-a" . nil)) ; universal-argument 키바인딩 때문
  :config
  (progn
    ;; syntax highlighting이 들어가니 가독성이 떨어져 org block에서는 끈다
    (setq org-src-fontify-natively nil)
    ;; fontify를 켜줘야 quote와 verse block도 배경 색상을 바꿀 수 있다
    (setq org-fontify-quote-and-verse-blocks t))

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
  (define-key org-mode-map [remap org-return] 'org-return-indent)

  ;; export시 underscore를 <sub></sub> subscripts로 변경하지 않게 한다
  (setq org-export-with-sub-superscripts nil)

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
    (advice-add 'org-clock-in
                :after (lambda (&rest _)
                         (org-todo "STARTED")))
    ;; 다른 org-clock 시작으로 clock-out 됐을 때, todo도 바꿔준다
    (add-hook 'org-clock-out-hook
              (lambda ()
                (when (and (boundp 'org-state)
                           (string= org-state "STARTED"))
                  (org-todo "DONE")))))

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

  ;; 기간 포맷으로 시간:분 사용. 24시가 넘어갈 때, 1d로 표현하는 게 보기 싫어서
  (setq org-duration-format (quote h:mm))

  ;; org-columns에서 effort를 볼 수 있게 추가
  (setq org-columns-default-format "%50ITEM(Task) %10Effort{:} %10CLOCKSUM")

  ;; org-clock-report 기본 프로퍼티
  (setq org-clock-clocktable-default-properties
        '(:maxlevel 2 :scope file :narrow 25! :properties ("effort")))

  (setq org-tag-alist '((:startgroup . nil)
                        ("task" . ?t)
                        ("codereview" . ?c)
                        ("meeting" . ?m) ; 회의
                        ("interview" . ?i) ; 면접
                        ("ftf" . ?f) ; 면담
                        ("wiki" . ?w)
                        (:endgroup . nil)
                        (:startgroup . nil)
                        ("til" . ?l)
                        (:endgroup . nil)
                        (:startgroup . nil)
                        ("followup" . ?u)
                        (:endgroup . nil)
                        (:startgroup . nil)
                        ("taskjuggler_project" . ?j)
                        ("taskjuggler_resource" . ?r)
                        (:endgroup . nil)
                        )
        )

  ;; capture
  (setq org-capture-templates
        '(("j"
           "Journal"
           entry
           (file+datetree+prompt "~/journal/2019.org")
           "* %?")))

  (org-babel-do-load-languages
   'org-babel-load-languages
   '((emacs-lisp . t)
     (js . t)
     (plantuml . t)
     (python . t)
     (elixir . t)
     (restclient . t)
     (C . t)))

  ;; <h가 #+BEGIN_HTML #+END_HTML에서 #+BEGIN_EXPORT html #+END_EXPORT로 변경됨
  ;; jekyll-org가 지원 안해서 예전으로 되돌림
  (setq org-structure-template-alist
        (delete '("h" . "export html") org-structure-template-alist))
  (add-to-list 'org-structure-template-alist '("h" . "html"))

  ;; plantuml
  (setq org-confirm-babel-evaluate nil)
  (setq org-plantuml-jar-path
        (expand-file-name "~/bin/plantuml.jar"))
  (add-hook 'org-babel-after-execute-hook
            (lambda ()
              (when org-inline-image-overlays
                (org-redisplay-inline-images))))
  (add-to-list 'org-structure-template-alist
               '("u" . "src plantuml :file ?.png\nskinparam monochrome true"))

  ;; org-mode 9.2에서 삭제된 < 키로 시작하는 template 삽입 기능을 되살리고자.
  ;; 예) <h TAB 입력시 #+BEGIN_HTML ... #+END_HTML 입력
  (require 'org-tempo)
  ;; <h TAB 눌렀을 때, org-tempo-keywords-alist가 우선권을 가져서 제거한다
  ;; 원했던 #+begin_html 대신 #+html: 가 나오는 문제 해결하려고
  (setq org-tempo-keywords-alist (delete '("H" . "html") org-tempo-keywords-alist)))

;;; https://github.com/krisajenkins/ob-translate
(use-package ob-translate
  :ensure t
  :config
  (setq ob-translate:default-dest "ko"))

;;; https://github.com/larstvei/ox-gfm
(use-package ox-gfm :ensure t)

;;; flymake
(global-set-key (kbd "C-S-p") 'flymake-goto-prev-error)
(global-set-key (kbd "C-S-n") 'flymake-goto-next-error)

;;; https://github.com/clojure-emacs/clojure-mode
(use-package clojure-mode :ensure t)

;;; https://github.com/emacs-lsp/lsp-mode
(use-package lsp-mode
  :ensure t
  :init
  (setq lsp-log-io t)
  :hook (elixir-mode . lsp)
  :config
  ;; 문서에 있는대로 map 이름을 lsp-mode-map 이렇게 그냥 쓰면 안 됨.
  ;; https://github.com/noctuid/evil-guide#why-dont-keys-defined-with-evil-define-key-work-immediately
  ;; 글을 참고해 lsp-mode-map 대신 'lsp-mode-map을 사용
  (evil-define-key 'motion 'lsp-mode-map (kbd "g r") 'lsp-find-references)
  :commands lsp)

(use-package lsp-ui :ensure t :commands lsp-ui-mode)
(use-package company-lsp :ensure t :commands company-lsp)
(use-package helm-lsp :ensure t :commands helm-lsp-workspace-symbol)
(use-package lsp-treemacs :ensure t :commands lsp-treemacs-errors-list)

;;; https://github.com/elixir-editors/emacs-elixir
(use-package elixir-mode
  :ensure t
  :config
  (add-hook 'elixir-mode-hook
            (lambda () (add-hook 'before-save-hook 'elixir-format nil t))))

;;; https://github.com/tonini/alchemist.el
(use-package alchemist
  :ensure t
  :config
  (add-hook 'alchemist-iex-mode-hook
            (lambda ()
              ;; evil-scroll-up과 충돌
              (define-key alchemist-iex-mode-map "\C-d" nil))))

;;; https://github.com/rejeep/el-mock.el
(use-package el-mock :ensure t)

;;; https://github.com/joshwnj/json-mode
(use-package json-mode :ensure t)

;;; https://github.com/OmniSharp/omnisharp-emacs
(use-package omnisharp
  :ensure t
  :config
  (add-hook 'csharp-mode-hook 'omnisharp-mode)
  (eval-after-load 'company '(add-to-list 'company-backends 'company-omnisharp))
  (if windows?
      (setq omnisharp-server-executable-path "C:\\omnisharp\\OmniSharp.exe"))
  (add-hook 'omnisharp-mode-hook
            (lambda ()
              (when omnisharp-mode
                (define-key evil-motion-state-local-map
                  (kbd "g d") 'omnisharp-go-to-definition))))
  (evil-define-key 'insert omnisharp-mode-map (kbd "M-.") 'omnisharp-auto-complete)
  (evil-define-key 'normal omnisharp-mode-map (kbd "g u") 'omnisharp-find-usages)
  (evil-define-key 'normal omnisharp-mode-map (kbd "g I") 'omnisharp-find-implementations) ; g i is taken
  (evil-define-key 'normal omnisharp-mode-map (kbd "g r") 'omnisharp-run-code-action-refactoring)
  (evil-define-key 'normal omnisharp-mode-map (kbd "g f") 'omnisharp-fix-code-issue-at-point)
  (evil-define-key 'normal omnisharp-mode-map (kbd "g F") 'omnisharp-fix-usings)
  (evil-define-key 'normal omnisharp-mode-map (kbd "g R") 'omnisharp-rename)
  (evil-define-key 'normal omnisharp-mode-map (kbd ", i") 'omnisharp-current-type-information)
  (evil-define-key 'normal omnisharp-mode-map (kbd ", I") 'omnisharp-current-type-documentation)
  (evil-define-key 'insert omnisharp-mode-map (kbd ".") 'omnisharp-add-dot-and-auto-complete)
  (evil-define-key 'normal omnisharp-mode-map (kbd ", n t") 'omnisharp-navigate-to-current-file-member)
  (evil-define-key 'normal omnisharp-mode-map (kbd ", n s") 'omnisharp-navigate-to-solution-member)
  (evil-define-key 'normal omnisharp-mode-map (kbd ", n f") 'omnisharp-navigate-to-solution-file-then-file-member)
  (evil-define-key 'normal omnisharp-mode-map (kbd ", n F") 'omnisharp-navigate-to-solution-file)
  (evil-define-key 'normal omnisharp-mode-map (kbd ", n r") 'omnisharp-navigate-to-region)
  (evil-define-key 'normal omnisharp-mode-map (kbd "<f12>") 'omnisharp-show-last-auto-complete-result)
  (evil-define-key 'insert omnisharp-mode-map (kbd "<f12>") 'omnisharp-show-last-auto-complete-result)
  (evil-define-key 'normal omnisharp-mode-map (kbd ",.") 'omnisharp-show-overloads-at-point)
  (evil-define-key 'normal omnisharp-mode-map (kbd ",rl") 'recompile))

;;; https://github.com/spotify/dockerfile-mode
(use-package dockerfile-mode :ensure t)

;;; https://github.com/mooz/js2-mode
(use-package js2-mode
  :ensure t
  :config
  (add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))
  ;; Better imenu
  (add-hook 'js2-mode-hook #'js2-imenu-extras-mode))

;;; cc-mode
(setq-default c-default-style "bsd"
              c-basic-offset 4)

;;; local package
(add-to-list 'load-path "~/.emacs.d/lisp/")
(require 'my-ox-confluence)
(require 'my-ox-taskjuggler)
(progn
  (load-file  "~/.emacs.d/lisp/taskjuggler-setting.el")
  (setq org-taskjuggler-reports-directory "~/taskjuggler")
  ;; 넉넉하게 잡아놔서 Error: Some tasks did not fit into the project time frame. 에러가 안 뜨게 한다
  (setq org-taskjuggler-default-project-duration 999)
  )

;; C-u 키바인딩을 evil에게 양보하고 가장 그럴듯한 키바인딩을 사용
(global-set-key (kbd "C-a") 'universal-argument)

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

(defun unity-open-editor-log ()
  (interactive)
  (let ((path (format "C:/Users/%s/AppData/Local/Unity/Editor/Editor.log"
                      (getenv "USERNAME"))))
    (if (file-exists-p path)
        (progn
          (find-file path)
          (auto-revert-tail-mode 1)
          (read-only-mode 1)
          (goto-char (point-max)))
      (message (concat "log file not found - " path)))))

(defun jekyll-default-image ()
  (interactive)
  (let ((name (format "{{ site.asseturl }}/%s-00.jpg"
                      (file-name-base (buffer-file-name)))))
    (kill-new name)
    (message "Copied default image name '%s' to the clipboard." name)))

(load-file "~/.emacs.d/lisp/my-gitlab.el")

(defun toggle-camelcase-underscores ()
  "Toggle between camelcase and underscore notation for the symbol at point."
  (interactive)
  (save-excursion
    (let* ((bounds (bounds-of-thing-at-point 'symbol))
           (start (car bounds))
           (end (cdr bounds))
           (currently-using-underscores-p (progn (goto-char start)
                                                 (re-search-forward "_" end t))))
      (if currently-using-underscores-p
          (progn
            (upcase-initials-region start end)
            (replace-string "_" "" nil start end)
            (downcase-region start (1+ start)))
        (replace-regexp "\\([A-Z]\\)" "_\\1" nil (1+ start) end)
        (downcase-region start (cdr (bounds-of-thing-at-point 'symbol)))))))

;;; evil http://www.emacswiki.org/emacs-en/Evil
(require 'evil)
(evil-mode 1)

;;; evil-paredit
(add-to-list 'load-path "~/.emacs.d/manual-package/evil-paredit")
(require 'evil-paredit)

;; ctrl+u 를 바인딩.
;; (setq evil-want-C-u-scroll t)가 동작 안해서 직접 정의
(define-key evil-motion-state-map (kbd "C-u") 'evil-scroll-up)

;;; evil-matchit
(require 'evil-matchit)
(global-evil-matchit-mode 1)

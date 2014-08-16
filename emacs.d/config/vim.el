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


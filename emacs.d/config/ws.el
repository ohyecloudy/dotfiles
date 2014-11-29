;; whitespace mode
(custom-set-faces 
 '(whitespace-line ((nil (:bold t :background "yellow"))))
 '(whitespace-trailing ((nil (:bold t :background "red1"))))
 '(whitespace-tab ((nil (:bold t :background "linen")))))

(global-whitespace-mode t)

(add-hook
 'after-change-major-mode-hook
 '(lambda ()
    (if (derived-mode-p 'prog-mode)
        (setq whitespace-line-column 80
              whitespace-style '(face tabs trailing lines-tail tab-mark))
      (setq whitespace-line-column nil
	    whitespace-style '(face tabs trailing tab-mark)))))

;; disable tabs mode
(setq-default indent-tabs-mode nil)

(add-hook 'before-save-hook 'delete-trailing-whitespace)


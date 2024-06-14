;;; doctor.el -*- lexical-binding: t; no-byte-compile: t; -*-

;;
;; helper
;;

(defun my-doctor--executable! (path install-guide)
  (unless (executable-find path)
    (error! "Couldn't find '%s'.\ninstallation guide\n%s" path install-guide)))

;;
;; diagnosis
;;

(unless (executable-find "fc-list")
  (error! "Couldn't find fc-list.")
  (pcase doom-system
    ('windows (error! "Need to install packages
pacman -S mingw64/mingw-w64-x86_64-gettext
pacman -S mingw64/mingw-w64-x86_64-fontconfig
"))
    ('macos   (error! "TODO Description of the `fc-list` program installation instructions"))
    ('linux   (error! "TODO Description of the `fc-list' program installation instructions"))))

;; Make sure required fonts are installed
;; ref: https://tecosaur.github.io/emacs-config/config.html
(let (required-fonts available-fonts missing-fonts)
  (setq required-fonts '("Consolas" "D2Coding"))

  (setq available-fonts
        (delete-dups
         (or (font-family-list)
             (and (executable-find "fc-list")
                  (with-temp-buffer
                    (call-process "fc-list" nil t nil ":" "family")
                    (split-string (buffer-string) "[,\n]"))))))

  (setq missing-fonts
        (delq nil (mapcar
                   (lambda (font)
                     (unless (delq nil (mapcar (lambda (f)
                                                 (string-match-p (format "^%s$" font) f))
                                               available-fonts))
                       font))
                   required-fonts)))

  (if available-fonts
      (dolist (font missing-fonts)
        (error! (format "Missing font: %s" font)))
    (error! "Unable to check for missing fonts, is fc-list installed?")))

(when (featurep :system 'windows)
  (my-doctor--executable! mermaid-mmdc-location
                          "cd ~/bin.local;mkdir mermaid-cli;cd mermaid-cli;npm install @mermaid-js/mermaid-cli")
  (my-doctor--executable! ob-mermaid-cli-path
                          "cd ~/bin.local;mkdir mermaid-cli;cd mermaid-cli;npm install @mermaid-js/mermaid-cli")
  (my-doctor--executable! "es"
                          "winget install --id=voidtools.Everything.Cli -e"))

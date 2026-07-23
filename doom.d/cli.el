;;; $DOOMDIR/cli.el -*- lexical-binding: t; no-byte-compile: t; -*-

;; On Windows, the tangle-triggered CLI restart aborts: native Emacs batch fails
;; the final stdout flush ("Write error to standard output"), discarding exit
;; code 254 so the launcher (bin/doom, bin/doom.ps1) never re-runs the next sync
;; step -- leaving packages uninstalled. Override the sync-time tangle to reload
;; packages in-process instead of restarting. Also pin the tangle coding system
;; to avoid a batch-mode `select-safe-coding-system' prompt.
(when (featurep :system 'windows)
  (defadvice! +literate-tangle--sync-no-restart-a ()
    :override #'+literate-tangle--sync
    (or (getenv "__NOTANGLE")
        (dlet ((coding-system-for-write 'utf-8-dos))
          (and (+literate-tangle +literate-config-file
                                 doom-module-config-file
                                 doom-user-dir)
               (or (not noninteractive)
                   (progn (doom-initialize-packages t) t)))))))

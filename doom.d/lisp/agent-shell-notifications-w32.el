;;; agent-shell-notifications-w32.el --- w32 provider for agent-shell-notifications -*- lexical-binding: t; -*-

;;; Commentary:

;; w32 native toast notification provider for agent-shell-notifications.
;; Uses `w32-notification-notify' (built-in on Windows Emacs).

;;; Code:

(defun agent-shell-notifications--send-w32 (plist)
  "Send a notification described by PLIST via `w32-notification-notify'."
  (w32-notification-notify
   :title (or (plist-get plist :title) "Agent Shell")
   :body (or (plist-get plist :body) "")
   :urgency 'normal))

(defun agent-shell-notifications--close-w32 (id)
  "Close the notification with ID via `w32-notification-close'."
  (w32-notification-close id))

(setq agent-shell-notifications-transform-timeout-function #'identity)
(setq agent-shell-notifications-transform-function #'identity)
(setq agent-shell-notifications-send-function
      #'agent-shell-notifications--send-w32)
(setq agent-shell-notifications-close-function
      #'agent-shell-notifications--close-w32)

(provide 'agent-shell-notifications-w32)

;;; agent-shell-notifications-w32.el ends here

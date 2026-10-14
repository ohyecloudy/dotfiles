;;; my-llm-usage.el --- LLM key budget usage from the company usage portal  -*- lexical-binding: t; -*-

;;; Commentary:

;; Fetch the LiteLLM key's budget usage from the usage portal, so the
;; agent-shell header can show it.  The gateway exposes spend only in
;; response headers that Claude Code discards, and the key may not call
;; LiteLLM's /key/info.  The portal is a free GET.
;;
;; The portal endpoint URL comes from auth-source:
;;
;;   machine llm-usage login url password https://PORTAL/v1/usage/me
;;
;; The key is the one Claude Code sends to the gateway.  The portal
;; accepts it only as a `key' query parameter.

;;; Code:

(require 'auth-source)
(require 'map)
(require 'parse-time)
(require 'seq)
(require 'subr-x)
(require 'url)
(require 'url-queue)
(require 'url-util)

(defconst my/llm-usage-auth-host "llm-usage"
  "auth-source machine name holding the usage portal settings.")

(defconst my/llm-usage-auth-url-login "url"
  "auth-source login whose password is the usage portal endpoint URL.")

(defconst my/llm-usage-key-env-vars '("ANTHROPIC_API_KEY" "ANTHROPIC_AUTH_TOKEN")
  "Environment variables searched in order for the gateway key.")

(defconst my/llm-usage-min-interval 60
  "Minimum seconds between portal requests.
The portal aggregates usage about once a minute, so polling faster
only adds load.")

(defconst my/llm-usage-warning-ratio 0.8
  "Spend/budget ratio at which the indicator is highlighted as a warning.")

(defvar my/llm-usage-latest nil
  "Latest `llm_usage' alist from the portal, or nil before a success.")

(defvar my/llm-usage-updated-at nil
  "Portal `llm_usage_updated_at' string for `my/llm-usage-latest'.")

(defvar my/llm-usage-last-error nil
  "Message of the last failed refresh, or nil after a success.")

(defvar my/llm-usage-updated-hook nil
  "Hook run after each refresh completes, successfully or not.")

(defvar my/llm-usage--last-request-time nil
  "`float-time' when the last request was sent, for throttling.")

(defvar my/llm-usage--in-flight nil
  "Non-nil while a request is outstanding.")

;;; Parsing and formatting

(defun my/llm-usage-parse (body)
  "Return (USAGE . UPDATED-AT) from the portal JSON string BODY.
USAGE is the `llm_usage' alist.  Signal an error when BODY is not
JSON or carries no `llm_usage' object."
  (let* ((json (json-parse-string body :object-type 'alist :null-object nil))
         (usage (and (consp json) (map-elt json 'llm_usage))))
    (unless (consp usage)
      (error "No llm_usage object in response"))
    (cons usage (map-elt json 'llm_usage_updated_at))))

(defun my/llm-usage--format-time (iso format-string)
  "Format the ISO 8601 string ISO with FORMAT-STRING, or nil if invalid."
  (when-let* (((stringp iso))
              (time (ignore-errors (parse-iso8601-time-string iso))))
    (format-time-string format-string time)))

(defun my/llm-usage--help-echo (usage updated-at error-message)
  "Return the tooltip for USAGE, UPDATED-AT and ERROR-MESSAGE."
  (let ((today (map-nested-elt usage '(daily_usage spend)))
        (reset (my/llm-usage--format-time
                (map-elt usage 'budget_reset_at) "%m-%d %H:%M"))
        (updated (my/llm-usage--format-time updated-at "%H:%M")))
    (string-join
     (delq nil
           (list (and (numberp today) (format "today $%.2f" today))
                 (and reset (format "resets %s" reset))
                 (and updated (format "updated %s" updated))
                 (and error-message
                      (format "last refresh failed: %s" error-message))))
     "\n")))

(defun my/llm-usage-format (usage updated-at error-message &optional face)
  "Return the header indicator for USAGE, e.g. \"$363.2/$500 (73%) 18d\".
UPDATED-AT and ERROR-MESSAGE go into the help-echo.  FACE is used
below `my/llm-usage-warning-ratio'.  Return \"$?\" when only
ERROR-MESSAGE is known, and nil when there is nothing to show."
  (let ((spend (map-elt usage 'total_spend))
        (budget (map-elt usage 'budget))
        (days (map-elt usage 'remaining_budget_reset_in_days)))
    (cond
     ((numberp spend)
      (let* ((ratio (and (numberp budget) (> budget 0)
                         (/ (float spend) budget)))
             (text (string-join
                    (delq nil
                          (list (if ratio
                                    (format "$%.1f/$%g" spend budget)
                                  (format "$%.1f" spend))
                                (and ratio
                                     (format "(%d%%)" (round (* 100 ratio))))
                                (and (numberp days) (format "%dd" days))))
                    " ")))
        (propertize text
                    'face (if (and ratio (>= ratio my/llm-usage-warning-ratio))
                              'warning
                            face)
                    'help-echo (my/llm-usage--help-echo
                                usage updated-at error-message))))
     (error-message
      (propertize "$?" 'face 'warning 'help-echo
                  (format "last refresh failed: %s" error-message))))))

(defun my/llm-usage-indicator (&optional face)
  "Return the header indicator for the latest usage, or nil.
FACE is used while usage is below `my/llm-usage-warning-ratio'."
  (my/llm-usage-format my/llm-usage-latest my/llm-usage-updated-at
                       my/llm-usage-last-error face))

;;; Fetching

(defun my/llm-usage--portal-url ()
  "Return the portal endpoint URL from auth-source, or nil."
  (when-let* ((entry (car (auth-source-search
                           :host my/llm-usage-auth-host
                           :user my/llm-usage-auth-url-login
                           :require '(:secret)
                           :max 1)))
              (secret (plist-get entry :secret)))
    (if (functionp secret) (funcall secret) secret)))

(defun my/llm-usage--api-key ()
  "Return the first non-empty key from `my/llm-usage-key-env-vars', or nil."
  (seq-some (lambda (var)
              (when-let* ((value (getenv var))
                          ((not (string-empty-p value))))
                value))
            my/llm-usage-key-env-vars))

(defun my/llm-usage--request-url (base key)
  "Return BASE with KEY appended as the `key' query parameter."
  (concat base (if (string-search "?" base) "&" "?")
          "key=" (url-hexify-string key)))

(defun my/llm-usage--finish (result error-message)
  "Store RESULT, a (USAGE . UPDATED-AT) cons, or ERROR-MESSAGE.
Then run `my/llm-usage-updated-hook'."
  (setq my/llm-usage--in-flight nil)
  (if error-message
      (setq my/llm-usage-last-error error-message)
    (setq my/llm-usage-latest (car result)
          my/llm-usage-updated-at (cdr result)
          my/llm-usage-last-error nil))
  (run-hooks 'my/llm-usage-updated-hook))

(defun my/llm-usage--response-body ()
  "Return the decoded body of the HTTP response in the current buffer."
  (goto-char (point-min))
  (unless (re-search-forward "\r?\n\r?\n" nil t)
    (error "No end of headers in response"))
  (decode-coding-string (buffer-substring-no-properties (point) (point-max))
                        'utf-8))

(defun my/llm-usage--handle-response (status)
  "Store the portal response in the current buffer, then kill it.
STATUS is the `url-retrieve' status plist.  Error messages never
include the request URL, which carries the key."
  (let ((outcome
         (condition-case err
             (if-let* ((http-error (plist-get status :error)))
                 (list :error (format "request failed: %S" http-error))
               (list :result (my/llm-usage-parse (my/llm-usage--response-body))))
           (error (list :error (format "invalid response: %s"
                                       (error-message-string err)))))))
    (when (buffer-live-p (current-buffer))
      (kill-buffer (current-buffer)))
    (my/llm-usage--finish (plist-get outcome :result)
                          (plist-get outcome :error))))

(defun my/llm-usage-refresh (&optional force)
  "Fetch usage from the portal asynchronously.
Skip when a request is outstanding, or when the last one was sent less
than `my/llm-usage-min-interval' seconds ago unless FORCE is non-nil.
Do nothing when the portal URL is not in auth-source.  Return non-nil
when a request was sent."
  (interactive (list t))
  (let ((base (my/llm-usage--portal-url)))
    (cond
     ((null base) nil)
     (my/llm-usage--in-flight nil)
     ((and (not force)
           my/llm-usage--last-request-time
           (< (- (float-time) my/llm-usage--last-request-time)
              my/llm-usage-min-interval))
      nil)
     (t
      (if-let* ((key (my/llm-usage--api-key)))
          (progn
            (setq my/llm-usage--in-flight t
                  my/llm-usage--last-request-time (float-time))
            ;; `url-queue-retrieve' always calls back, with an error
            ;; status when the request fails or times out.
            (url-queue-retrieve (my/llm-usage--request-url base key)
                                #'my/llm-usage--handle-response nil t t)
            t)
        (my/llm-usage--finish
         nil (format "no key in %s"
                     (string-join my/llm-usage-key-env-vars " or ")))
        nil)))))

(provide 'my-llm-usage)
;;; my-llm-usage.el ends here

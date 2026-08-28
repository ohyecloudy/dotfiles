;;; my-ox-taskjuggler-test.el --- Tests for my-ox-taskjuggler -*- lexical-binding: t; -*-

(require 'ert)
(require 'org)
(require 'my-ox-taskjuggler)

(defun my/ox-taskjuggler-test--build-task (org-text)
  "Export ORG-TEXT with the taskjuggler backend and return the plan string."
  (org-export-string-as org-text 'taskjuggler))

(ert-deftest my/my-ox-taskjuggler-test/complete-done-is-100 ()
  "DONE heading yields \"complete 100\"."
  (let ((plan (my/ox-taskjuggler-test--build-task "\
* project :taskjuggler_project:
** DONE finished task
")))
    (should (string-match-p "complete 100" plan))))

(ert-deftest my/my-ox-taskjuggler-test/complete-property-is-used ()
  "Explicit COMPLETE property is preserved."
  (let ((plan (my/ox-taskjuggler-test--build-task "\
* project :taskjuggler_project:
** TODO half done
:PROPERTIES:
:COMPLETE: 50
:END:
")))
    (should (string-match-p "complete 50" plan))))

(ert-deftest my/my-ox-taskjuggler-test/complete-defaults-to-0 ()
  "Non-DONE task without COMPLETE defaults to \"complete 0\"."
  (let ((plan (my/ox-taskjuggler-test--build-task "\
* project :taskjuggler_project:
** TODO not started
")))
    (should (string-match-p "complete 0" plan))
    (should-not (string-match-p "complete 100" plan))))

(provide 'my-ox-taskjuggler-test)
;;; my-ox-taskjuggler-test.el ends here

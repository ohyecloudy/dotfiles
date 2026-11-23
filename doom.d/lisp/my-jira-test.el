;;; my-jira-test.el --- Tests for my-jira  -*- lexical-binding: t; -*-

(require 'ert)
(require 'my-jira)

(ert-deftest my/jira-test/issue-id-plain ()
  (should (equal (my/jira--issue-id "https://example.atlassian.net/browse/WINDI-2175")
                 "WINDI-2175")))

(ert-deftest my/jira-test/issue-id-ignores-query ()
  (should (equal (my/jira--issue-id "https://example.atlassian.net/browse/WINDI-2175?xpis=abc")
                 "WINDI-2175")))

(ert-deftest my/jira-test/issue-id-ignores-fragment ()
  (should (equal (my/jira--issue-id "https://example.atlassian.net/browse/WINDI-2175#comment")
                 "WINDI-2175")))

(ert-deftest my/jira-test/remove-query ()
  (should (equal (my/jira--remove-query "https://example.atlassian.net/browse/WINDI-2175?xpis=abc")
                 "https://example.atlassian.net/browse/WINDI-2175")))

(ert-deftest my/jira-test/remove-query-keeps-plain-url ()
  (should (equal (my/jira--remove-query "https://example.atlassian.net/browse/WINDI-2175")
                 "https://example.atlassian.net/browse/WINDI-2175")))

;;; my-jira-test.el ends here

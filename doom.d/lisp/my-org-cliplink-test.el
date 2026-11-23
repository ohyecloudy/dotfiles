;;; my-org-cliplink-test.el --- Tests for my-org-cliplink  -*- lexical-binding: t; -*-

(require 'ert)
(require 'my-org-cliplink)

(ert-deftest my/org-cliplink-test/refine-jira-removes-query ()
  (should (equal (my/org-cliplink--refine-url
                  "https://example.atlassian.net/browse/WINDI-2175?xpis=abc")
                 "https://example.atlassian.net/browse/WINDI-2175")))

(ert-deftest my/org-cliplink-test/refine-jira-keeps-plain-url ()
  (should (equal (my/org-cliplink--refine-url
                  "https://example.atlassian.net/browse/WINDI-2175")
                 "https://example.atlassian.net/browse/WINDI-2175")))

(ert-deftest my/org-cliplink-test/refine-confluence-with-title ()
  (should (equal (my/org-cliplink--refine-url
                  "https://example.atlassian.net/wiki/spaces/ABC/pages/123/Title?atlOrigin=x")
                 "https://example.atlassian.net/wiki/pages/viewpage.action?pageId=123")))

(ert-deftest my/org-cliplink-test/refine-confluence-query-without-title ()
  (should (equal (my/org-cliplink--refine-url
                  "https://example.atlassian.net/wiki/spaces/ABC/pages/123?atlOrigin=x")
                 "https://example.atlassian.net/wiki/pages/viewpage.action?pageId=123")))

;;; my-org-cliplink-test.el ends here

;;; my-org-cliplink-confluence-test.el --- Tests for my-org-cliplink-confluence  -*- lexical-binding: t; -*-

(require 'ert)
(require 'my-org-cliplink-confluence)

(ert-deftest my/org-cliplink-confluence-test/cloud-page-id-with-title ()
  (should (equal (funcall (my/org-cliplink-confluence--extract-page-id-fun :cloud)
                          "https://example.atlassian.net/wiki/spaces/ABC/pages/123/Title")
                 "123")))

(ert-deftest my/org-cliplink-confluence-test/cloud-page-id-ignores-query ()
  (should (equal (funcall (my/org-cliplink-confluence--extract-page-id-fun :cloud)
                          "https://example.atlassian.net/wiki/spaces/ABC/pages/123?atlOrigin=x")
                 "123")))

(ert-deftest my/org-cliplink-confluence-test/v6-page-id ()
  (should (equal (funcall (my/org-cliplink-confluence--extract-page-id-fun 6)
                          "https://example.com/confluence/pages/viewpage.action?pageId=456")
                 "456")))

;;; my-org-cliplink-confluence-test.el ends here

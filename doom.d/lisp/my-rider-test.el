;;; my-rider-test.el --- Tests for my-rider  -*- lexical-binding: t; -*-

(require 'ert)
(require 'my-rider)

(defmacro my/rider-test--with-tree (files &rest body)
  "Create FILES (relative paths) under a temp dir bound to `root', run BODY."
  (declare (indent 1))
  `(let ((root (file-name-as-directory (make-temp-file "my-rider-test" t))))
     (unwind-protect
         (progn
           (dolist (f ,files)
             (let ((path (expand-file-name f root)))
               (make-directory (file-name-directory path) t)
               (write-region "" nil path nil 'silent)))
           ,@body)
       (delete-directory root t))))

(defun my/rider-test--names (paths)
  "Return the non-directory parts of PATHS."
  (mapcar #'file-name-nondirectory paths))

;;; Project discovery

(ert-deftest my/rider-test/uproject-beats-sln ()
  (my/rider-test--with-tree '("Game/Game.uproject" "Game/Game.sln" "Game/Source/a.cpp")
    (should (equal (my/rider-test--names
                    (my/rider-find-project (expand-file-name "Game/Source/a.cpp" root)))
                   '("Game.uproject")))))

(ert-deftest my/rider-test/slnx-beats-sln ()
  (my/rider-test--with-tree '("App/App.slnx" "App/App.sln" "App/a.cs")
    (should (equal (my/rider-test--names
                    (my/rider-find-project (expand-file-name "App/a.cs" root)))
                   '("App.slnx")))))

(ert-deftest my/rider-test/nearest-project-wins ()
  (my/rider-test--with-tree '("Outer.uproject" "sub/Inner.sln" "sub/deep/a.cs")
    (should (equal (my/rider-test--names
                    (my/rider-find-project (expand-file-name "sub/deep/a.cs" root)))
                   '("Inner.sln")))))

(ert-deftest my/rider-test/multiple-candidates-same-extension ()
  (my/rider-test--with-tree '("App/B.sln" "App/A.sln" "App/a.cs")
    (should (equal (my/rider-test--names
                    (my/rider-find-project (expand-file-name "App/a.cs" root)))
                   '("A.sln" "B.sln")))))

(ert-deftest my/rider-test/no-project ()
  (my/rider-test--with-tree '("plain/a.txt")
    (should (null (my/rider-find-project (expand-file-name "plain/a.txt" root))))))

;;; Default candidate

(ert-deftest my/rider-test/default-candidate-matches-dir-name ()
  (should (equal (my/rider--default-candidate
                  "c:/work/App/" '("c:/work/App/Tools.sln" "c:/work/App/app.sln"))
                 "c:/work/App/app.sln")))

(ert-deftest my/rider-test/default-candidate-falls-back-to-first ()
  (should (equal (my/rider--default-candidate
                  "c:/work/App/" '("c:/work/App/A.sln" "c:/work/App/B.sln"))
                 "c:/work/App/A.sln")))

;;; Executable selection

(ert-deftest my/rider-test/latest-executable ()
  (should (equal (my/rider--latest-executable
                  '("C:/Program Files/JetBrains/JetBrains Rider 2025.3.3/bin/rider64.exe"
                    "C:/Program Files/JetBrains/JetBrains Rider 2026.1/bin/rider64.exe"
                    "C:/Program Files/JetBrains/JetBrains Rider EAP/bin/rider64.exe"))
                 "C:/Program Files/JetBrains/JetBrains Rider 2026.1/bin/rider64.exe")))

(ert-deftest my/rider-test/latest-executable-none-parsable ()
  (should (null (my/rider--latest-executable
                 '("C:/Program Files/JetBrains/JetBrains Rider EAP/bin/rider64.exe"))))
  (should (null (my/rider--latest-executable nil))))

;;; Command arguments

(ert-deftest my/rider-test/command-args-with-project ()
  (should (equal (my/rider--command-args "c:/work/App/App.sln" "c:/work/App/a.cs" 12 5)
                 '("c:\\work\\App\\App.sln" "--line" "12" "--column" "5"
                   "c:\\work\\App\\a.cs"))))

(ert-deftest my/rider-test/command-args-without-project ()
  (should (equal (my/rider--command-args nil "c:/work/a.cs" 1 1)
                 '("--line" "1" "--column" "1" "c:\\work\\a.cs"))))

;;; my-rider-test.el ends here

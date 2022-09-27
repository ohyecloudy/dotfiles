;;; ob-cs.el --- org-babel functions for csharp evaluation

;; Copyright (C) 2011-2015 Free Software Foundation, Inc.

;; Original Author: Eric Schulte (ob-java.el)
;; Author: thomas "at" friendlyvillagers.com
;; Keywords: literate programming, reproducible research
;; Homepage: http://orgmode.org

;; This file is NOT YET part of GNU Emacs.

;; GNU Emacs is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Currently this only supports the external compilation and execution
;; of csharp code blocks (i.e., no session support).

;;; Code:
(require 'ob)

(defvar org-babel-tangle-lang-exts)
(add-to-list 'org-babel-tangle-lang-exts '("cs" . "csx"))

(defcustom org-babel-csharp-command "dotnet-script"
  "Name of the csharp command.
May be either a command in the path, like mono
or an absolute path name, like /usr/local/bin/mono
parameters may be used, like mono -verbose"
  :group 'org-babel
  :version "24.3"
  :type 'string)

(defun org-babel-execute:cs (body params)
  (let* ((full-body (org-babel-expand-body:generic body params))
         (cmpflag (or (cdr (assoc :cmpflag params)) ""))
         (cmdline (or (cdr (assoc :cmdline params)) ""))
         (src-file (org-babel-temp-file "csharp-src-" ".csx"))
         (exe-file (concat (file-name-sans-extension src-file)  ".csx")))
         (with-temp-file  src-file (insert full-body))
    (let ((results (org-babel-eval (concat org-babel-csharp-command " " exe-file) "")))
      (org-babel-reassemble-table
       (org-babel-result-cond (cdr (assoc :result-params params))
         (org-babel-read results)
         (let ((tmp-file (org-babel-temp-file "c-")))
           (with-temp-file tmp-file (insert results))
           (org-babel-import-elisp-from-file tmp-file)))
       (org-babel-pick-name
        (cdr (assoc :colname-names params)) (cdr (assoc :colnames params)))
       (org-babel-pick-name
        (cdr (assoc :rowname-names params)) (cdr (assoc :rownames params)))))))

(defun org-babel-prep-session:cs (session params)
  "Return an error because csharp does not support sessions."
  (error "Sessions are not (yet) supported for CSharp"))


(provide 'ob-cs)
;;; ob-cs.el ends here

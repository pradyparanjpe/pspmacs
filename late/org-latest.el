;;;; pspmacs-org.el --- org-mode -*- lexical-binding: t; -*-

;; Copyright (C) 2023  Pradyumna Swanand Paranjape

;; Author: Pradyumna Swanand Paranjape <pradyparanjpe@rediffmail.com>
;; Keywords: help, languages

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU Lesser General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU Lesser General Public License for more details.

;; You should have received a copy of the GNU Lesser General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;;; Code:


(use-package org
  ;; :straight (:type built-in)
  :ensure t)

(defun pspmacs/load-suitable (fname &optional nag)
  "Load emacs init file FNAME.

If FNAME is found, load it and return.
If org/el counterpart of FNAME is found, load it and return.
To load,

If extension `string='s 'org', use function `org-babel-load-file'.
If extension `string='s 'el', use function `load'

If nothing is found and if NAG is `t', throw error. Default: return"
  (cond
   ((string= (file-name-extension fname) "org")
    (cond ((file-readable-p fname)
           (org-babel-load-file fname))
          ((file-readable-p (file-name-with-extension fname "el"))
           (load (file-name-with-extension fname "el") nil 'nomessage))))
   ((string= (file-name-extension fname) "el")
    (cond ((file-readable-p fname)
           (load fname nil 'nomessage))
          ((file-readable-p (file-name-with-extension fname "org"))
           (org-babel-load-file (file-name-with-extension fname "el")))))
   (nag (user-error (format "Neither %s.{el,org} found."
                            (file-name-sans-extension fname))))))

(defun pspmacs/load-inherit (&optional fname)
  "Inherit all equivalent files.

Re-definition of early-loaded function after the correct orgmode is loaded.
Files may be placed in `pvt-emacs-directory' and/or `local-emacs-directory'.
If FNAME is supplied, *that* corresponding file name is attempted, else,
stem of `load-file-name' is attempted.
Init files are loaded using the function `pspmacs-load-suitable'.
Settings loaded from files located in `pvt-emacs-directory' are overwritten
by settings loaded from files located in `local-emacs-directory'."
  (let ((name-branch
     (file-relative-name (or fname load-file-name) user-emacs-directory)))
    (dolist (config-dir `(,pvt-emacs-directory ,local-emacs-directory) nil)
  (let* ((modular-init (expand-file-name name-branch config-dir)))
    (if (file-exists-p modular-init)
        (pspmacs/load-suitable modular-init))))))

(pspmacs/load-inherit)
(provide 'pspmacs-org-latest)

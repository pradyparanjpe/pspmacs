;;;; org-latest.el --- org-mode -*- lexical-binding: t; -*-

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

Function defined in early/definitions.el is hereby redefined to enable
`org-babel-load-file' method, now that the correct org-mode is loaded.

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
           (org-babel-load-file (file-name-with-extension fname "org")))))
   (nag (user-error (format "Neither %s.{el,org} found."
                            (file-name-sans-extension fname))))))

(pspmacs/load-inherit)

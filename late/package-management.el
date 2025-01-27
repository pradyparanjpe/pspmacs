;;; late/package-management.el --- initiate package manager -*- lexical-binding: t; -*-

;; Copyright © 2023-2024  Pradyumna Swanand Paranjape

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

;; Commentary: Set packate manager system, Also, initiate latest org mode.:
;; Code:

(customize-set-variable
 'package-user-dir (expand-file-name "packages" local-emacs-dir))

(load
 (expand-file-name "bootstrap.el" pspmacs/packaging-directory) nil 'nomessage)
(pspmacs/init-package-manager)

(use-package org
  :demand t)

(dolist (init-dir pspmacs/worktrees)
  (when-let* ((default-directory (expand-file-name "modules/" init-dir))
              ((file-directory-p default-directory)))
    (add-to-list 'load-path default-directory)
    (normal-top-level-add-subdirs-to-load-path)))

(pspmacs/load-inherit)

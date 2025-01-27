;;; pspmacs-editing-enhancement.el --- writing aid -*- lexical-binding: t; -*-

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

;;; Commentary:

;;; Code:

(use-package csv-mode
  :mode ("\\.[ct]sv\\'" . csv-mode)
  :general
  (pspmacs/local-leader-keys
    csv-mode-map
    "a" '(:ignore t :wk "lign")
    "aa" '(csv-align-fields :wk "lign")
    "au" '(csv-unalign-fields :wk "nalign")

    "," '(:ignore t :wk ":Separator")
    ",v" '(csv-toggle-invisibility "isibility toggle")

    "s" '(:ignore t :wk "ort")
    "ss" '(csv-sort-fields :wk "ort")
    "sn" '(csv-sort-numeric-fields :wk "umeric")

    "r" '(csv-reverse-region :wk "everse")
    "T" '(csv-transpose :wk "ranspose")

    "y" '(:ignore t :wk "ank")
    "yy" '(csv-kill-fields :wk ":Kill")
    "yc" '(csv-yank-fields :wk "opy"))
  :custom
  (csv-comment-start "#")
  :config
  :hook
  (csv-mode . csv-align-mode)
  (csv-mode . pspmacs/csv-highlight)
  (csv-mode . csv-guess-set-separator)
  (csv-mode . csv-header-line))

(use-package doc-view
  :custom
  (doc-view-cache-directory
   (file-name-as-directory (xdg/make-path "doc-view" 'state)))
  :hook
  (doc-view-minor-mode . auto-revert-mode))

(pspmacs/load-inherit)
;;; pspmacs-editing-enhancement.el ends here

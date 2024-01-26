;;; pspmacs-editing-enhancement.el --- writing aid -*- lexical-binding: t; -*-

;; Copyright © 2023  Pradyumna Swanand Paranjape

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
    "a" '(:ignore t :wk "align")
    "aa" '(csv-align-fields :wk "align")
    "au" '(csv-unalign-fields :wk "align")

    "," '(:ignore t :wk "separator")
    ",v" '(csv-toggle-invisibility "visibility toggle")

    "s" '(:ignore t :wk "sort")
    "ss" '(csv-sort-fields :wk "sort")
    "sn" '(csv-sort-numeric-fields :wk "numeric")

    "r" '(csv-reverse-region :wk "reverse")
    "T" '(csv-transpose :wk "transpose")

    "y" '(:ignore t :wk "yank")
    "yy" '(csv-kill-fields :wk "kill")
    "yc" '(csv-yank-fields :wk "yank"))
  :custom
  (csv-comment-start "#")
  :config
  :hook
  (csv-mode . csv-align-mode)
  (csv-mode . pspmacs/csv-highlight)
  (csv-mode . csv-guess-set-separator)
  (csv-mode . csv-header-line))

(pspmacs/load-inherit)
;;; pspmacs-editing-enhancement.el ends here

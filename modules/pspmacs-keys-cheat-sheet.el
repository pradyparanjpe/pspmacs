;;; pspmacs-funcs.el --- Keybinding maps using pspmacs-general.el -*- lexical-binding: t; -*-

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

(use-package which-key
  :after evil
  :init
  (which-key-mode)
  :custom
  (which-key-separator "")
  (which-key-prefix-prefix "")
  (which-key-allow-multiple-replacements t)
  :config
  (dolist (special '(("SPC"       . "␣")
                     ("TAB"       . "↹")
                     ("RET"       . "⏎")
                     ("ESC"       . "⎋")
                     ("backspace" . "⌫")
                     ("DEL"       . "⌦")))
    (add-to-list 'which-key-replacement-alist
                 `((,(car special) . nil) . (,(cdr special) . nil))))
  (which-key-setup-minibuffer))

(pspmacs/load-inherit)
;;; pspmacs-keys-cheat-sheet.el ends here

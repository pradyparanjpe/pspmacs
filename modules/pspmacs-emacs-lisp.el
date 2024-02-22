;;;; pspmacs-emacs-lisp.el --- emacs-lisp mode config  -*- lexical-binding: t; -*-

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

(add-hook 'emacs-lisp-mode-hook #'pspmacs/prettify-emacs-lisp)
(add-hook 'emacs-lisp-mode-hook #'pspmacs/setup-elisp)
;;; pspmacs-emacs-lisp.el ends here

(defun pspmacs/el-package-provide ()
  (interactive)
  (let ((base (file-name-sans-extension
               (file-name-nondirectory (buffer-file-name)))))
    (insert (format "(provide '%s)\n;;; %s.el ends here" base base))))

(pspmacs/local-leader-keys
  :keymaps 'emacs-lisp-mode-map
  "TAB" '(indent-sexp)
  "e" '(:ignore t :wk "eval")
  "ee" '(eval-last-sexp :wk "sexp")
  "ex" '(eval-expression :wk "expression")
  "eb" '(eval-buffer :wk "buffer"))

(pspmacs/load-inherit)

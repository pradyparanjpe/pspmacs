;;; rust.el --- rust ide -*- lexical-binding: t; -*-

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

(use-package rust-mode
  :mode ("\\.rs\\'" . rust-mode)
  :interpreter ("rust" . rust-mode)
  :general
  (pspmacs/leader-keys
    :states 'normal
    :keymaps 'rust-mode-map
    "Cc" '(rust-compile :wk "compile")
    "Ck" '(rust-check :wk "check")
    "Ct" '(rust-test :wk "test")
    "Cr" '(rust-run :wk "run"))
  (pspmacs/local-leader-keys
    :states 'normal
    :keymaps 'rust-mode-map
    "ll" '(rust-run-clippy :wk "lint")
    "dw" '(rust-dbg-wrap-or-unwrap :wk "wrap/unwrap dbg!"))
  :custom
  (indent-tabs-mode nil)
  (rust-format-on-save t)
  :hook
  ((rust-mode . pspmacs/prettify-rust)
   (rust-mode . eglot-ensure)))

(use-package cargo-mode
  :custom
  (compilation-scroll-output t)
  :general
  (pspmacs/local-leader-keys
    :states 'normal
    :keymaps 'cargo-minor-mode-map
    "r" '(cargo-mode-command-map :wk "cargo"))
  :hook
  (rust-mode . cargo-minor-mode))

(pspmacs/load-inherit)
;;; pspmacs-rust.el ends here

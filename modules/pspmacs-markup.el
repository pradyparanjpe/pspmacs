;;; pspmacs-markup.el --- filesystem markup -*- lexical-binding: t; -*-

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

(use-package yaml-mode
  :mode ("\\.ya?ml\\'" . yaml-mode)
  :general
  (:keymaps 'yaml-mode-map
            "\C-m" 'newline-and-indent))

(use-package toml-mode
  :mode ("\\.toml\\'" . toml-mode))

(use-package markdown-mode
  :mode ("README\\.md\\'" . gfm-mode)
  :custom (markdown-command "multimarkdown"))

(use-package mermaid-mode)
(use-package ob-mermaid)

(customize-set-variable 'oer-reveal-org-includes-dir local-emacs-dir)
(use-package emacs-reveal
  :ensure nil
  :vc (:fetcher "gitlab" :repo "oer/emacs-reveal")
  :general
  (pspmacs/local-leader-keys
    :keymaps 'org-mode-map
    "v" '(:ignore t :wk "reveal")
    "vv" '(org-pandoc-export-to-revealjs :wk "export")
    "vo" '(org-pandoc-export-to-revealjs-and-open :wk "export and open"))
  :custom
  (oer-reveal-org-includes-dir (expand-file-name "oer-reveal-org" local-emacs-dir))
  (org-re-reveal-single-file t)
  :hook (org-mode . reveal-mode))

(pspmacs/load-inherit)

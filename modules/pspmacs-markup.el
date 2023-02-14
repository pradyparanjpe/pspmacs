;;;; pspmacs-markup.el --- filesystem markup -*- lexical-binding: t; -*-

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

(use-package yaml-mode
  :general
  (:keymaps 'yaml-mode-map
        "\C-m" '(newline-and-indent)))

(use-package toml-mode)

(use-package markdown-mode
  :mode ("README\\.md\\'" . gfm-mode)
  :custom (markdown-command "multimarkdown"))

(setq auctex-kw
        (if (string= pspmacs/package-manager 'builtin)
            '(:ensure auctex)
            '(:straight auctex)))

  (setq tex-kwargs
        `(tex
          ,@auctex-kw
          :mode ("\\.tex\\'" . LaTeX-mode)
          ;; :ensure auctex
          ;; :straight auctex
          :general
          (pspmacs/local-leader-keys
            :keymaps 'LaTeX-mode-map
            "=" '(reftex-toc :wk "reftex toc")
            "(" '(reftex-latex :wk "reftex label")
            ")" '(reftex-reference :wk "reftex ref")
            "m" '(LaTeX-macro :wk "insert macro")
            "s" '(LaTeX-section :wk "insert section header")
            "e" '(LaTeX-environment :wk "insert environment")
            "p" '(preview-at-point :wk "preview at point")
            "f" '(TeX-font :wk "font")
            "c" '(TeX-command-run-all :wk "compile"))
          :custom
          (TeX-parse-self t); parse on load
          (reftex-plug-into-AUCTeX t)
          (TeX-auto-save t)  ; parse on save
          (TeX-source-correlate-mode t)
          (TeX-source-correlate-method 'synctex)
          (TeX-source-correlate-start-server nil)
          (TeX-electric-sub-and-superscript t)
          (TeX-engine 'xetex) ;; use xetex by default
          (TeX-save-query nil)
          :config
          (add-to-list 'TeX-view-program-selection '(output-pdf "PDF Tools"))
          (add-to-list 'TeX-view-program-selection '(output-pdf "Zathura"))
          (add-hook 'TeX-after-compilation-finished-functions
                    #'TeX-revert-document-buffer)
          (sp-with-modes
              '(tex-mode plain-tex-mode latex-mode)

            (sp-local-pair "\\(" "\\)"
                           :unless '(sp-point-before-word-p
                                     sp-point-before-same-p
                                     sp-latex-point-after-backslash)
                           :trigger-wrap "$"
                           :trigger "$")

            (sp-local-pair "\\[" "\\]"
                           :unless '(sp-point-before-word-p
                                     sp-point-before-same-p
                                     sp-latex-point-after-backslash)))
          :hook
          ((LaTeX-mode . (reftex-mode olivetti-mode prettify-symbols-mode
                                      outline-minor-mode turn-on-auto-fill)))))

  (eval `(use-package ,@tex-kwargs))
;; (add-hook 'TeX-mode-hook #'flymake-aspell-setup)

(use-package evil-tex
  :hook (LaTeX-mode . evil-tex-mode))

(use-package citar
  :no-require
  :general
  (pspmacs/local-leader-keys
    :keymaps 'org-mode-map
    "@" '(:ignore :wk "cite")
    "@i" '(org-cite-insert :wk "insert"))
  :custom
  (org-cite-insert-processor 'citar)
  (org-cite-follow-processor 'citar)
  (org-cite-activate-processor 'citar)
  (citar-bibliography org-cite-global-bibliography))

(when pspmacs/install-git-clones
  (pspmacs/install-git-clone '(emacs-reveal
                               :type git
                               :host gitlab
                               :repo "oer/emacs-reveal"))
  (use-package emacs-reveal
    :ensure nil
    :load-path (lambda ()
                 (unless (string= pspmacs/package-manager "straight")
                   (expand-file-name "org-auctex" pspmacs/crooked-dir)))
    :general
    (pspmacs/local-leader-keys
      :keymaps 'org-mode-map
      "v" '(:ignore t :wk "reveal")
      "vv" '(org-pandoc-export-to-revealjs :wk "export")
      "vo" '(org-pandoc-export-to-revealjs-and-open :wk "export and open"))
    :custom
    (oer-reveal-org-includes-dir (expand-file-name
                                  "oer-reveal-org" local-emacs-directory))
    :config
    (setq org-re-reveal-single-file t)
    :hook (org-mode . reveal-mode)))

(pspmacs/load-inherit)

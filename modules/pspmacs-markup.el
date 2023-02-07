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

(use-package org-auctex
  :straight (:type git :host github :repo
                   "karthink/org-auctex")
  :hook (org-mode . org-auctex-mode))

(use-package auctex
  :no-require t
  :mode ("\\.tex\\'" . LaTeX-mode)
  :init
  (setq TeX-parse-self t ; parse on load
        reftex-plug-into-AUCTeX t
        TeX-auto-save t  ; parse on save
        TeX-source-correlate-mode t
        TeX-source-correlate-method 'synctex
        TeX-source-correlate-start-server nil
        TeX-electric-sub-and-superscript t
        TeX-engine 'luatex ;; use lualatex by default
        TeX-save-query nil))

(use-package latex
  :straight auctex
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
  :init
  (setq TeX-electric-math (cons "\\(" "\\)"))
  ;; (setq preview-scale-function 1.5) ;; too big on vivacia
  :config
  ;; (add-hook 'TeX-mode-hook #'visual-line-mode)
  (add-hook 'TeX-mode-hook #'reftex-mode)
  (add-hook 'TeX-mode-hook #'olivetti-mode)
  (add-hook 'TeX-mode-hook #'turn-on-auto-fill)
  (add-hook 'TeX-mode-hook #'prettify-symbols-mode)
  (add-hook 'TeX-after-compilation-finished-functions
            #'TeX-revert-document-buffer)
  (add-to-list 'TeX-view-program-selection '(output-pdf "PDF Tools"))
  (add-hook 'TeX-mode-hook #'outline-minor-mode)
  ;; (add-hook 'TeX-mode-hook #'flymake-aspell-setup)
  (add-to-list 'TeX-view-program-selection '(output-pdf "Zathura")))

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

(use-package emacs-reveal
  :straight (:type git :host gitlab :repo "oer/emacs-reveal")
  :ensure t
  :general
  (pspmacs/local-leader-keys
    :keymaps 'org-mode-map
    "v" '(:ignore t :wk "reveal")
    "vv" '(org-pandoc-export-to-revealjs :wk "export")
    "vo" '(org-pandoc-export-to-revealjs-and-open :wk "export and open"))
  :config
  (setq org-re-reveal-single-file t)
  :hook (org-mode . reveal-mode))

(defun pspmacs/project-to-publish-alist
    (org-root html-root org-templates)
  "Set root locations for source ORG-ROOT and target HTML-ROOT

to publish orgmode files to html."
  (interactive
   (let (org-root html-root org-templates)
     (setq org-root (read-directory-name
             "ORG Directory:\t"
             nil default-directory
             ".*" nil))
     (setq html-root (read-directory-name
          "HTML Directory:\t"
          (expand-file-name "../html" org-root) nil
          ".*" nil))
     (setq org-templates (read-directory-name
              "Templates Directory:\t"
              (expand-file-name "templates"
                        pspmacs/org-template-path)
              nil ".*" nil))
     (list org-root html-root org-templates)))

  (catch 'pspmacs/mk-tag
    (unless (file-directory-p html-root)
  (if (yes-or-no-p (format "%s doesn't exist. Create? " html-root))
      (make-directory html-root t)
    (throw 'pspmacs/mk-tag nil)))
    (setq org-publish-project-alist
      (list
       (list "org-notes"
         :base-directory org-root
         :base-extension "org"
         :publishing-directory html-root
         :recursive t
         :publishing-function 'org-html-publish-to-html
         :headline-levels 4
         :auto-preamble t)
       (list "org-static"
         :base-directory org-root
         :base-extension
         "css\\|js\\|png\\|jpg\\|gif\\|pdf\\|mp3\\|ogg\\|swf"
         :publishing-directory html-root
         :recursive t
         :publishing-function 'org-publish-attachment)
       (list "org-templates"
         :base-directory org-templates
         :base-extension
         "css\\|js\\|png\\|jpg\\|gif\\|pdf\\|mp3\\|ogg\\|swf"
         :publishing-directory html-root
         :recursive t
         :publishing-function 'org-publish-attachment)
       (list "org" :components
         '("org-notes" "org-static" "org-templates"))))))

(pspmacs/load-inherit)

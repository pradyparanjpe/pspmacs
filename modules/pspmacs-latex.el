;;; pspmacs-latex.el --- LaTeX -*- lexical-binding: t; -*-

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

(defun karthink/latex-with-outline ()
  (add-to-list 'minor-mode-overriding-map-alist
               `(outline-minor-mode . ,outline-minor-mode-map))
  (outline-minor-mode 1))

(setq auctex-kw
      (if (string= pspmacs/package-manager 'builtin)
          '(:ensure auctex)
        '(:straight auctex)))

(setq tex-kwargs
      `(latex
        ,@auctex-kw
        :hook ((LaTeX-mode . smartparens-mode)
               (LaTeX-mode . karthink/latex-with-outline))
        :mode ("\\.tex\\'" . LaTeX-mode)
        :defines (TeX-auto-save
                  TeX-parse-self
                  TeX-electric-escape
                  TeX-PDF-mode
                  TeX-source-correlate-method
                  TeX-newline-function
                  TeX-view-program-list
                  TeX-view-program-selection
                  TeX-mode-map)
        :general
        (pspmacs/local-leader-keys
          :keymaps 'LaTeX-mode-map
          "TAB" '(TeX-complete-symbol :wk "complete symbol")
          "=" '(reftex-toc :wk "reftex toc")
          "(" '(reftex-latex :wk "reftex label")
          ")" '(reftex-reference :wk "reftex ref")
          "c" '(TeX-command-run-all :wk "compile")
          "e" '(LaTeX-environment :wk "insert environment")
          "f" '(TeX-font :wk "font")
          "i" '(LaTeX-insert-item :wk "insert item")
          "m" '(LaTeX-macro :wk "insert macro")
          "p" '(preview-at-point :wk "preview at point")
          "s" '(LaTeX-section :wk "insert section header"))
        :custom
        (TeX-auto-save t)  ; parse on save
        (TeX-parse-self t) ; parse on load
        (TeX-electric-escape nil) ; for preview
        (TeX-PDF-mode nil)
        (TeX-error-overview-open-after-TeX-run nil)
        (TeX-engine 'xetex) ;; use xetex by default
        (TeX-source-correlate-start-server nil)
        (TeX-source-correlate-mode t)
        (TeX-source-correlate-method 'synctex)
        (TeX-newline-function 'reindent-then-newline-and-indent)
        (TeX-electric-sub-and-superscript t)
        (TeX-PDF-from-DVI "Dvips")
        (TeX-save-query nil)
        :config
        (add-to-list 'TeX-view-program-selection '(output-pdf "pdf-tools"))
        (add-to-list 'TeX-view-program-selection '(output-pdf "zathura"))
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
                                   sp-latex-point-after-backslash)))))

(eval `(use-package ,@tex-kwargs))
;; (add-hook 'TeX-mode-hook #'flymake-aspell-setup)

(defun karthink/preview-scale-larger ()
  "Increase the size of `preview-latex' images"
  (setq preview-scale-function
        (lambda nil (* 1.25 (funcall (preview-scale-from-face))))))

(setq preview-kwargs
      `(preview
        :after latex
        ,@auctex-kw
        :hook (LaTeX-mode . karthink/preview-scale-larger)
        :general
        (pspmacs/local-leader-keys :keymaps 'LaTeX-mode-map
          "p" 'preview-map)))
(eval `(use-package ,@preview-kwargs))

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

(use-package reftex
  :after latex
  :commands turn-on-reftex
  :hook ((latex-mode LaTeX-mode) . turn-on-reftex)
  :custom
  (reftex-default-bibliography (lambda ()
                                 (remq 'nil
                                       (mapcar
                                        (lambda (x)
                                          (let
                                              ((bibfile
                                                (expand-file-name "biblio.bib" x)))
                                 (if (file-exists-p bibfile) bibfile)))
                   pspmacs/ref-paths))))
  (reftex-insert-label-flags '("sf" "sfte"))
  (reftex-plug-into-AUCTeX t)
  (reftex-use-multiple-selection-buffers t))

(use-package cdlatex
  :after latex
  ;; :commands turn-on-cdlatex
  :hook (LaTeX-mode . turn-on-cdlatex)
  :general
  (pspmacs/local-leader-keys
    :keymaps 'org-cdlatex-mode-map
    "c"  '(:ignore t :wk "cdlatex")
    "ce" '(org-cdlatex-environment-indent :wk "environment"))
  :custom
  (cdlatex-math-symbol-alist '((?F ("\\Phi"))
                               (?o ("\\omega" "\\mho" "\\mathcal{O}"))
                               (?. ("\\cdot" "\\circ"))
                               (?6 ("\\partial"))
                               (?v ("\\vee" "\\forall"))
                               (?^ ("\\uparrow" "\\Updownarrow" "\\updownarrow"))))
  (cdlatex-math-modify-alist '((?b "\\mathbb" "\\textbf" t nil nil)
                               (?B "\\mathbf" "\\textbf" t nil nil)
                               (?t "\\text" nil t nil nil)))
  (cdlatex-paired-parens "$[{(")
  :config
  (dolist (cmd '(("vc" "Insert \\vect{}" "\\vect{?}"
                  cdlatex-position-cursor nil nil t)
                 ("sfr" "Insert \\sfrac{}{}" "\\sfrac{?}{}"
                  cdlatex-position-cursor nil nil t)

                 ("abs" "Insert \\abs{}" "\\abs{?}"
                  cdlatex-position-cursor nil nil t)
                 ("equ*" "Insert equation* env"
                  "\\begin{equation*}\n?\n\\end{equation*}"
                  cdlatex-position-cursor nil t nil)
                 ("sn*" "Insert section* env"
                  "\\section*{?}"
                  cdlatex-position-cursor nil t nil)
                 ("ss*" "Insert subsection* env"
                  "\\subsection*{?}"
                  cdlatex-position-cursor nil t nil)
                 ("sss*" "Insert subsubsection* env"
                  "\\subsubsection*{?}"
                  cdlatex-position-cursor nil t nil)))
    (push cmd cdlatex-command-alist))
  (cdlatex-reset-mode))

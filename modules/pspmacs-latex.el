;;; pspmacs-latex.el --- LaTeX -*- lexical-binding: t; -*-

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

(defun karthink/latex-with-outline ()
  (add-to-list 'minor-mode-overriding-map-alist
               `(outline-minor-mode . ,outline-minor-mode-map))
  (outline-minor-mode 1))

(use-package latex
  :after tex
  :ensure auctex
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
                             sp-latex-point-after-backslash))))

(defun karthink/add-latex-in-org-mode-expansions ()
  "Extend org-mode expansions with LaTeX"
  ;; Make Emacs recognize \ as an escape character in org
  (modify-syntax-entry ?\\ "\\" org-mode-syntax-table)
  ;; Paragraph end at end of math environment
  (setq paragraph-start (concat paragraph-start "\\|\\\\end{\\([A-Za-z0-9*]+\\)}"))
  ;; (setq paragraph-separate (concat paragraph-separate "\\|\\\\end{\\([A-Za-z0-9*]+\\)}"))
  ;; Latex mode expansions
  (with-eval-after-load 'expand-region
    (set (make-local-variable 'karthink/try-expand-list)
         (append (remove #'karthink/mark-method-call karthink/try-expand-list)
                 '(LaTeX-mark-environment
                   karthink/mark-LaTeX-inside-math
                   karthink/mark-latex-inside-delimiters
                   karthink/mark-latex-outside-delimiters
                   karthink/mark-LaTeX-math)))))

(use-package ox-latex
 :ensure org
 :after ox
 :custom
 (org-export-with-LaTeX-fragments t)
 (org-export-with-smart-quotes t)
 (org-latex-caption-above nil)
 (org-latex-tables-booktabs t)
 (org-latex-prefer-user-labels t)
 (org-latex-reference-command "\\cref{%s}")
 (org-latex-compiler "xelatex")
 (org-latex-pdf-process
  '("latexmk -pdflatex='%latex -shell-escape -interaction nonstopmode' -pdf -output-directory=%o -f %f"))

 ;; From https://git.tecosaur.net/tec/emacs-config,
 ;; the default link colors are hideous.
 (org-latex-hyperref-template
  "
\\usepackage{xcolor}
\\providecolor{url}{HTML}{006fcf}
\\providecolor{link}{HTML}{6f2f47}
\\providecolor{cite}{HTML}{8f8f2f}
\\hypersetup{
  pdfauthor={%a},
  pdftitle={%t},
  pdfkeywords={%k},
  pdfsubject={%d},
  pdfcreator={%c},
  pdflang={%L},
  breaklinks=true,
  colorlinks=true,
  linkcolor=link,
  urlcolor=url,
  citecolor=cite}
\\urlstyle{same}
%% hide links styles in toc
\\NewCommandCopy{\\oldtoc}{\\tableofcontents}
\\renewcommand{\\tableofcontents}{\\begingroup\\hypersetup{hidelinks}\\oldtoc\\endgroup}
")

 :hook
 (org-mode . karthink/add-latex-in-org-mode-expansions)

 :config
 (dolist (package '(("" "longtable" nil)
                    ("" "booktabs"  nil)
                    ("" "color"     nil)
                    ("" "cancel"    t)))
   ;; ;FIXME: Some documentclasses load these themselves,
   ;; ;causing all manner of conflicts.
   ;; ("capitalize" "cleveref"  nil)
   ;; (""           "amsmath"   t)
   ;; (""           "amssymb"   t)
   (cl-pushnew package org-latex-packages-alist
               :test (lambda (a b) (equal (cadr a) (cadr b)))))
 (let* ((article-sections '(("\\section{%s}"       . "\\section*{%s}")
                            ("\\subsection{%s}"    . "\\subsection*{%s}")
                            ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
                            ("\\paragraph{%s}"     . "\\paragraph*{%s}")
                            ("\\subparagraph{%s}"  . "\\subparagraph*{%s}"))))
   (pcase-dolist (`(,name ,class-string . ,extra)
                   `(("IEEEtran" "\\documentclass[conference]{IEEEtran}")
                     ("article" "\\documentclass{scrartcl}")
                     ("report" "\\documentclass{scrreprt}")
                     ("blank" "[NO-DEFAULT-PACKAGES]\n[NO-PACKAGES]\n[EXTRA]")
                     ("book" "\\documentclass[twoside=false]{scrbook}"
                      ("\\chapter{%s}" . "\\chapter*{%s}"))))
     (setf (alist-get name org-latex-classes nil nil #'equal)
           (append (list class-string) extra article-sections)))))

(defun karthink/preview-scale-larger ()
  "Increase the size of `preview-latex' images"
  (setq preview-scale-function
        (lambda nil (* 1.25 (funcall (preview-scale-from-face))))))

(use-package preview
  :after latex
  :ensure auctex
  :hook (LaTeX-mode . karthink/preview-scale-larger)
  :general
  (pspmacs/local-leader-keys :keymaps 'LaTeX-mode-map "p" 'preview-map))

(use-package evil-tex
  :hook (LaTeX-mode . evil-tex-mode))

(use-package bibtex
  :custom
  ;; Following customizations are suggested by org-ref in their wiki
  (bibtex-autokey-year-length 4)
  (bibtex-autokey-name-year-separator "-")
  (bibtex-autokey-year-title-separator "-")
  (bibtex-autokey-titleword-separator "-")
  (bibtex-autokey-titlewords 2)
  (bibtex-autokey-titlewords-stretch 1)
  (bibtex-autokey-titleword-length 5)
  (bibtex-completion-bibliography
   (remq 'nil (mapcar
               (lambda (x)
                 (let ((bibfile (expand-file-name "biblio.bib" x)))
                   (if (file-exists-p bibfile) bibfile)))
               pspmacs/ref-paths)))
  (bibtex-completion-library-path
   (remq 'nil (mapcar
               (lambda (x)
                 (let ((bibdir (file-name-as-directory
                                (expand-file-name "library" x))))
                   (if (file-exists-p bibdir) bibdir)))
               pspmacs/ref-paths)))
  (bibtex-completion-notes-path
   (car (last (remq 'nil (mapcar
                          (lambda (x)
                            (let ((bibdir (file-name-as-directory
                                           (expand-file-name "notes" x))))
                              (if (file-exists-p bibdir) bibdir)))
                          pspmacs/ref-paths)))))
  (biblio-download-directory
   (car (last (remq 'nil
                    (mapcar
                     (lambda (x)
                       (let ((bibdir (file-name-as-directory
                                      (expand-file-name "downloads" x))))
                         (if (file-exists-p bibdir) bibdir)))
                     pspmacs/ref-paths))))))

(use-package reftex
  :after (latex bibtex)
  :commands turn-on-reftex
  :hook ((latex-mode LaTeX-mode) . turn-on-reftex)
  :custom
  (reftex-default-bibliography org-cite-global-bibliography)
  (reftex-insert-label-flags '("sf" "sfte"))
  (reftex-plug-into-AUCTeX t)
  (reftex-use-multiple-selection-buffers t))

(use-package org-ref
  :after (org bibtex)
  :demand t
  :general
  (pspmacs/local-leader-keys :keymaps 'bibtex-mode-map
    "i" '(:ignore t :wk "insert")
    "ir" '(:ignore t :wk "org-ref")
    "irh" '(org-ref-bibtex-hydra/body :wk "hydra"))

  (pspmacs/local-leader-keys :keymaps 'org-mode-map
    "i" '(:ignore t :wk "insert")
    "ir" '(:ignore t :wk "org-ref")
    "irl" '(org-ref-insert-link :wk "link")
    "irh" '(org-ref-insert-link-hydra/body :wk "hydra"))

  :config
  ;; Initialize components
  (require 'org-ref-arxiv)
  (require 'org-ref-scopus)
  (require 'org-ref-wos)

  :custom
  (org-ref-bibtex-pdf-download-directory
   (pcase (type-of bibtex-completion-library-path)
     (string bibtex-completion-library-path)
     (_ (car (last bibtex-completion-library-path))))))

(use-package citar
  :after latex
  :demand t
  :general
  (pspmacs/local-leader-keys
    :keymaps 'latex-mode-map
    "@i" '(citar-insert-citation :wk "insert"))
  (pspmacs/local-leader-keys
    :keymaps 'org-mode-map
    "@i" '(org-cite-insert :wk "insert")
    "@c" '(citar-copy-citation :wk "insert"))
  :custom
  (citar-bibliography org-cite-global-bibliography)
  (citar-at-point-function 'embark-act)
  (citar-file-open-function #'consult-file-externally)
  (org-cite-insert-processor 'citar)
  (org-cite-follow-processor 'citar)
  (org-cite-activate-processor 'citar))

(use-package cdlatex
  :after latex org
  ;; :commands turn-on-cdlatex
  :hook
  ((org-mode . turn-on-org-cdlatex)
   (LaTeX-mode . turn-on-cdlatex))
  :general
  (pspmacs/local-leader-keys
    :keymaps 'org-cdlatex-mode-map
    "c"  '(:ignore t :wk "cdlatex")
    "c`" '(cdlatex-math-symbol :wk "symbol")
    "c_" '(org-cdlatex-underscore-caret :wk "sub-superscript")
    "ce" '(org-cdlatex-environment-indent :wk "environment"))
  :custom
  (cdlatex-math-symbol-prefix (kbd "M-+") "unbind cdlatex-math-symbol")
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

(use-package pdf-tools
  :config
  ;; initialise
  (pdf-tools-install)
  ;; open pdfs scaled to fit width
  ;; use normal isearch
  :general
  (general-def 'normal pdf-view-mode-map
    (kbd "C-s") 'isearch-forward)
  :custom
  (pdf-view-display-size 'fit-width)
  (pdf-annot-activate-created-annotations t "automatically annotate highlights"))

(pspmacs/load-inherit)

;;; pspmacs-latex.el --- LaTeX -*- lexical-binding: t; -*-

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

(defun pspmacs/setup-org-latex-headers ()
  "Set up headers for org latex exports."
  (let ((pkgs-xtra
         (concat "[NO-DEFAULT-PACKAGES]\n" "[PACKAGES]\n" "[EXTRA]\n")))
    (dolist (class-args
             `(("blank" "[NO-DEFAULT-PACKAGES]\n[NO-PACKAGES]\n[EXTRA]")
               ("apa6" "\\documentclass{apa6}")
               ("IEEEtran" "\\documentclass[conference]{IEEEtran}")
               ("article" "\\documentclass[11pt]{article}")
               ("komaarticle" "\\documentclass{scrartcl}")
               ("elsarticle" ,(concat "\\documentclass{elsarticle}" pkgs-xtra))
               ("report" "\\documentclass[11pt]{report}")
               ("komareport" "\\documentclass{scrreprt}")
               ("mimore" ,(concat "\\documentclass{mimore}" pkgs-xtra))
               ("moderncv" "\\documentclass{moderncv}")
               ("book" "\\documentclass[11pt]{book}")
               ("komabook" "\\documentclass[twoside=false]{scrbook}" nil
                '("\\chapter{%s}" . "\\chapter*{%s}"))
               ("mimosis"
                ,(concat
                  "\\documentclass{mimosis}\n"
                  pkgs-xtra
                  "\\newcommand{\\mboxparagraph}[1]"
                  "{\\paragraph{#1}\\mbox{}\\\\}\n"
                  "\\newcommand{\\mboxsubparagraph}[1]\n"
                  "{\\subparagraph{#1}\\mbox{}\\\\}")
                :no-defaults
                ("\\chapter{%s}"          . "\\chapter*{%s}")
                ("\\section{%s}"          . "\\section*{%s}")
                ("\\subsection{%s}"       . "\\subsection*{%s}")
                ("\\subsubsection{%s}"    . "\\subsubsection*{%s}")
                ("\\mboxparagraph{%s}"    . "\\mboxparagraph*{%s}")
                ("\\mboxsubparagraph{%s}" . "\\mboxsubparagraph*{%s}"))
               ("beamer"
                ,(concat "\\documentclass[presentation]{beamer}\n" pkgs-xtra))))
      (apply #'pspmacs/org-latex--add-class class-args))))

(defun karthink/add-latex-in-org-mode-expansions ()
  "Extend org-mode expansions with LaTeX"
  ;; Make Emacs recognize \ as an escape character in org
  (modify-syntax-entry ?\\ "\\" org-mode-syntax-table)
  ;; Paragraph end at end of math environment
  (setq paragraph-start
        (concat paragraph-start "\\|\\\\end{\\([A-Za-z0-9*]+\\)}"))
  ;; (setq paragraph-separate
  ;;   (concat paragraph-separate "\\|\\\\end{\\([A-Za-z0-9*]+\\)}"))
  ;; LaTeX mode expansions
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
  :general
  (pspmacs/local-leader-keys
    :keymaps 'org-mode-map
    "xp"  '(org-latex-export-to-pdf :wk "df")
    "xb" '(org-beamer-export-to-pdf :wk "eamer"))
  :init
  (defun pspmacs/org-latex--add-class
      (name class-str &optional no-defaults &rest sections)
    "Add CLASS-STR and SECTIONS for documentclass NAME.

CLASS-STR may contain other arbitrary header declarations.
Modify the variable `org-latex-classes'.
If NO-DEFAULTS, only declare SECTIONS, ordinarily, use default sections."
    (setf (alist-get name org-latex-classes nil nil #'equal)
          (append (list class-str)
                  sections
                  (unless no-defaults
                    '(("\\section{%s}"       . "\\section*{%s}")
                      ("\\subsection{%s}"    . "\\subsection*{%s}")
                      ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
                      ("\\paragraph{%s}"     . "\\paragraph*{%s}")
                      ("\\subparagraph{%s}"  . "\\subparagraph*{%s}"))))))
  :custom
  (org-export-with-LaTeX-fragments t)
  (org-export-with-smart-quotes t)
  (org-latex-caption-above nil)
  (org-latex-tables-booktabs t)
  (org-latex-prefer-user-labels t)
  (org-latex-reference-command "\\cref{%s}")
  (org-latex-compiler "xelatex")
  (org-latex-src-block-backend 'listings)
  (org-latex-to-mathml-convert-command
   "latexmlmath '%i' --presentationmathml=%o")
  (org-latex-pdf-process
   '("latexmk -pdflatex='%latex -shell-escape -interaction nonstopmode' -pdf -output-directory=%o -f %f"
     "bibtex %b"
     "makeindex %b"
     "latexmk -pdflatex='%latex -shell-escape -interaction nonstopmode' -pdf -output-directory=%o -f %f"
     "latexmk -pdflatex='%latex -shell-escape -interaction nonstopmode' -pdf -output-directory=%o -f %f"))

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

  (org-startup-with-latex-preview t)
  (org-highlight-latex-and-related '(native))
  (org-preview-latex-default-process 'dvisvgm)

  :hook
  (org-mode . karthink/add-latex-in-org-mode-expansions)

  :config
  (plist-put org-format-latex-options :background "Transparent")
  (plist-put org-format-latex-options :scale 1.5)
  (plist-put org-format-latex-options :zoom 1.0)

  (dolist (package '(("" "longtable" nil)
                     ("" "booktabs"  nil)
                     ("" "listings"  nil)
                     ("" "minted"    nil)
                     ("" "color"     nil)
                     ("" "cancel"    t)))
    ;; ;FIXME: Some documentclasses load these themselves,
    ;; ;causing all manner of conflicts.
    ;; ("capitalize" "cleveref"  nil)
    ;; (""           "amsmath"   t)
    ;; (""           "amssymb"   t)
    (cl-pushnew package org-latex-packages-alist
                :test (lambda (a b) (equal (cadr a) (cadr b)))))
  (pspmacs/setup-org-latex-headers))

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
    "TAB" '(TeX-complete-symbol :wk ":Complete symbol")
    "=" '(reftex-toc :wk ":Reftex toc")
    "(" '(reftex-latex :wk ":Reftex label")
    ")" '(reftex-reference :wk ":Reftex ref")
    "c" '(TeX-command-run-all :wk "ompile")
    "e" '(LaTeX-environment :wk "nvironment insert")
    "f" '(TeX-font :wk "ont")
    "i" '(LaTeX-insert-item :wk "tem insert")
    "m" '(LaTeX-macro :wk "acro insert")
    "p" '(preview-at-point :wk "review at point")
    "s" '(LaTeX-section :wk "ection head insert"))
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

(defun pspmacs/rename-beamer-export (export-command &rest r)
  "Intended as advice around `org-beamer-export-to-pdf'.

Back up already existing files by the extensions
\\='.tex\\=' \\='.aux\\=' \\='.log\\=' \\='.pdf\\=' by adding \\='.bak\\='.

Call the wrapped function. Catch any error thrown.

Restore backed up files."
  (let ((file-exts '(".tex" ".aux" ".log" ".pdf")))
    ;; Back up old files
    (seq-doseq (ext file-exts)
      (let ((old-name (org-export-output-file-name ext)))
        (when (file-exists-p old-name)
          (rename-file
           old-name (concat (file-name-sans-extension old-name) ext ".bak")
           t))))

    ;; Call wrapped function
    (condition-case nil
        (apply export-command r)
      (error (message "Error while calling command %s." export-command)))

    ;; Restore backed up files
    (seq-doseq (ext file-exts)
      (let ((export-name (org-export-output-file-name ext))
            (backup-name (org-export-output-file-name (concat ext ".bak"))))
        (when (file-exists-p export-name)
          (rename-file
           export-name (concat (file-name-sans-extension export-name)
                               "_present" ext)
           t))
        (when (file-exists-p backup-name)
          (rename-file
           backup-name (file-name-sans-extension backup-name)
           t))))))

(advice-add 'org-beamer-export-to-pdf :around #'pspmacs/rename-beamer-export)

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
  (bibtex-autokey-titleword-length 5))

(use-package bibtex-completion
  :after bibtex
  :custom
  ;; Following customizations are suggested by org-ref in their wiki
  (bibtex-completion-bibliography
   (remq 'nil (mapcar (lambda (x)
                        (let ((bibfile (expand-file-name "biblio.bib" x)))
                          (if (file-exists-p bibfile) bibfile)))
                      pspmacs/ref-paths)))
  (bibtex-completion-library-path
   (remq 'nil (mapcar (lambda (x)
                        (let ((bibdir (file-name-as-directory
                                       (expand-file-name "library" x))))
                          (if (file-exists-p bibdir) bibdir)))
                      pspmacs/ref-paths)))
  (bibtex-completion-notes-path
   (car (last (remq 'nil (mapcar
                          (lambda (x)
                            (let ((bibdir (file-name-as-directory
                                           (expand-file-name "notes" x))))
                              (when (file-exists-p bibdir) bibdir)))
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
  :commands turn-on-reftex
  :hook ((latex-mode LaTeX-mode) . turn-on-reftex)
  :custom
  (reftex-default-bibliography org-cite-global-bibliography)
  (reftex-insert-label-flags '("sf" "sfte"))
  (reftex-plug-into-AUCTeX t)
  (reftex-use-multiple-selection-buffers t))

(use-package org-ref
  :demand t
  :after (org bibtex-completion)
  :general
  (pspmacs/local-leader-keys :keymaps 'bibtex-mode-map
    "i" '(:ignore t :wk "nsert")
    "ir" '(:ignore t :wk "ef")
    "irh" '(org-ref-bibtex-hydra/body :wk "ydra"))

  (pspmacs/local-leader-keys :keymaps 'org-mode-map
    "i" '(:ignore t :wk "nsert")
    "ir" '(:ignore t :wk "ef")
    "irl" '(org-ref-insert-link :wk "ink")
    "irh" '(org-ref-insert-link-hydra/body :wk "ydra"))

  :init
  (require 'org-ref)
  :config
  ;; Initialize components
  (require 'org-ref-arxiv)
  (require 'org-ref-scopus)
  (require 'org-ref-wos)
  (require 'org-ref-pubmed)
  (require 'org-ref-sci-id)
  :custom
  (org-ref-bibtex-pdf-download-directory
   (pcase (type-of bibtex-completion-library-path)
     (string bibtex-completion-library-path)
     (_ (car (last bibtex-completion-library-path))))))

(use-package org-ref-prettify
  :disabled
  :after org-ref
  :hook
  (org-mode . org-ref-prettify))

(use-package citar
  :after latex
  :demand t
  :general
  (pspmacs/local-leader-keys
    :keymaps 'latex-mode-map
    "@i" '(citar-insert-citation :wk "nsert"))
  (pspmacs/local-leader-keys
    :keymaps 'org-mode-map
    "@i" '(org-cite-insert :wk "nsert")
    "@c" '(citar-copy-citation :wk "opy"))
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
    "c"  '(:ignore t :wk "dlatex")
    "c`" '(cdlatex-math-symbol :wk ":Symbol")
    "c_" '(org-cdlatex-underscore-caret :wk ":Sub-superscript")
    "ce" '(org-cdlatex-environment-indent :wk "nvironment"))
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
  :init
  (defun pspmacs/pdf-view--disable-incompatible ()
    "Disable modes declared in variable `pdf-view-incompatible-modes'."
    (interactive)
    (dolist (incompat pdf-view-incompatible-modes)
      (when (and (boundp incompat) incompat)
        (funcall incompat -1))))
  :config
  ;; initialise
  (pdf-tools-install)
  ;; open pdfs scaled to fit width
  (setq-default pdf-view-display-size 'fit-width)
  ;; use normal isearch
  :general
  (general-def 'normal pdf-view-mode-map
    (kbd "C-s") 'isearch-forward)
  :custom
  (pdf-view-display-size 'fit-width)
  (pdf-annot-activate-created-annotations t "automatically annotate highlights")
  :hook
  (pdf-view-mode . pspmacs/pdf-view--disable-incompatible)
  (pdf-view-mode . auto-revert-mode))

(use-package nov)
(use-package djvu)

(use-package org-pdftools
  :hook (org-mode . org-pdftools-setup-link))

(use-package org-noter-pdftools
  :after (org-noter nov djvu)
  :config
  ;; Add a function to ensure precise note is inserted
  (defun org-noter-pdftools-insert-precise-note (&optional toggle-no-questions)
    (interactive "P")
    (org-noter--with-valid-session
     (let ((org-noter-insert-note-no-questions
            (if toggle-no-questions
                (not org-noter-insert-note-no-questions)
              org-noter-insert-note-no-questions))
           (org-pdftools-use-isearch-link t)
           (org-pdftools-use-freepointer-annot t))
       (org-noter-insert-note (org-noter--get-precise-info)))))

  ;; fix https://github.com/weirdNox/org-noter/pull/93/commits/f8349ae7575e599f375de1be6be2d0d5de4e6cbf
  (defun org-noter-set-start-location (&optional arg)
    "When opening a session with this document, go to the current location.
With a prefix ARG, remove start location."
    (interactive "P")
    (org-noter--with-valid-session
     (let ((inhibit-read-only t)
           (ast (org-noter--parse-root))
           (location (org-noter--doc-approx-location
                      (when (called-interactively-p 'any) 'interactive))))
       (with-current-buffer (org-noter--session-notes-buffer session)
         (org-with-wide-buffer
          (goto-char (org-element-property :begin ast))
          (if arg
              (org-entry-delete nil org-noter-property-note-location)
            (org-entry-put nil org-noter-property-note-location
                           (org-noter--pretty-print-location location))))))))
  (with-eval-after-load 'pdf-annot
    (add-hook 'pdf-annot-activate-handler-functions
              #'org-noter-pdftools-jump-to-note)))

(pspmacs/load-inherit)

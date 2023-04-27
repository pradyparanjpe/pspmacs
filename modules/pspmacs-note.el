;;;; pspmacs-note.el --- org-mode -*- lexical-binding: t; -*-

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

(defun org-cdlatex-pbb (&rest _arg)
  "Execute `cdlatex-pbb' in LaTeX fragments.
Revert to the normal definition outside of these fragments."
  (interactive "P")
  (if (org-inside-LaTeX-fragment-p)
      (call-interactively 'cdlatex-pbb)
    (let (org-cdlatex-mode)
      (call-interactively (key-binding (vector last-input-event))))))

(use-package org
  :mode ("\\.org\\'" . org-mode)
  :general
  ;; ORG AGENDA
  (pspmacs/leader-keys
    :keymaps 'org-mode-map
    "ao"  '(:ignore t :wk "org-agenda")
    "ao#" '(org-agenda-list-stuck-projects :wk "stuck")
    "ao/" '(org-occur-in-agenda-files :wk "occur in agenda")
    "aoa" '(org-agenda-list :wk "list")
    "aoc" '(org-agenda-capture :wk "capture")
    "aoo" '(org-agenda :wk "capture")
    "aot" '(org-todo-list :wk "todo")

    "t"   '(:ignore t :wk "time")
    "tc"  '(:ignore t :wk "clock")
    "tcc" '(org-clock-cancel :wk "cancel")
    "tci" '(org-clock-in :wk "in")
    "tco" '(org-clock-out :wk "out")
    "tcj" '(org-clock-goto :wk "goto"))

  ;; ORG TABLE
  (pspmacs/local-leader-keys
    :keymaps 'org-mode-map
    "TAB"    '(:ignore t :wk "table")

    "TAB ?"  '(org-table-field-info :wk "field info")
    "TAB H"  '(org-table-move-column-left :wk "move column left")
    "TAB K"  '(org-table-move-row-up :wk "move row up")
    "TAB J"  '(org-table-move-row-down :wk "move row down")
    "TAB L"  '(org-table-move-column-right :wk "move column right")

    "TAB d"  '(:ignore t :wk "delete")
    "TAB dc" '(org-table-delete-column :wk "column")
    "TAB dd" '(org-table-blank-field :wk "field contents")
    "TAB dr" '(org-table-kill-row :wk "row")

    "TAB h"  '(org-table-previous-field :wk "prev field")

    "TAB i"  '(:ignore t :wk "insert")
    "TAB iH" '(org-table-hline-and-move :wk "¯¯¯¯")
    "TAB ic" '(org-table-insert-column :wk "column")
    "TAB ih" '(org-table-insert-hline :wk "____")
    "TAB ii" '(table-insert :wk "table")
    "TAB ir" '(org-table-insert-row :wk "row")

    "TAB j"  '(org-table-previous-row :wk "prev row")
    "TAB k"  '(org-table-next-row :wk "next row")
    "TAB l"  '(org-table-next-field :wk "next field")

    "TAB p"  '(:ignore t :wk "plot")
    "TAB pa" '(orgtbl-ascii-plot :wk "ascii")
    "TAB pg" '(org-plot/gnuplot :wk "gnuplot")
    "TAB pp" '(org-plot/gnuplot :wk "gnuplot")

    "TAB s"  '(org-table-sort-lines :wk "sort"))

  (pspmacs/local-leader-keys
    :keymaps 'org-mode-map
    "@"   '(:ignore t :wk "reference")

    "="   '(:ignore t :wk "count")
    "=w"  '(:ignore t :wk "words")
    "=ww" '(count-words t :wk "all")
    "=wr" '(count-words-region t :wk "region")

    ">"   '(org-demote-subtree :wk "demote subtree")
    "<"   '(org-promote-subtree :wk "demote subtree")

    "["   '(:ignore t :wk "checkboxes")
    "[]"  '(pspmacs/org-put-checkboxes :wk "here")
    "[*"  '(pspmacs/org-put-checkboxes-recursively
            :wk "recursively")
    "[!"  '(:ignore t :wk "remove")
    "[!]" '((lambda () (interactive)
              (pspmacs/org-put-checkboxes t))
            :wk "insert here")
    "[!*" '((lambda () (interactive)
              (pspmacs/org-put-checkboxes-recursively t))
            :wk "insert here")

    "S"   '(:ignore :wk "special")
    "Sx"  '(org-cut-special :wk "org cut special")
    "Se"  '(org-edit-special :wk "edit")

    "b"   '(:keymap org-babel-map :wk "babel")
    "f"   '(org-footnote-action :wk "footnote action")

    "d"   '(:ignore t :wk "date-time")
    "dd"  '(org-deadline :wk "date-time")
    "dT"  '(org-time-stamp-inactive :wk "inactive time stamp")
    "ds"  '(org-schedule :wk "schedule")
    "dt"  '(org-time-stamp :wk "time-stamp")

    "L"   '(:ignore t :wk "link")
    "Lp"  '(pspmacs/org-paste-as-link :wk "paste")
    "LL"  '(org-store-link t :wk "grab")
    "Ll"  '(org-insert-link t :wk "put")
    "Ly"  '(pspmacs/org-copy-link-at-point :wk "yank")

    "i"   '(:ignore t :wk "insert")
    "ih"  '(org-insert-heading :wk "insert heading")
    "is"  '(org-insert-subheading :wk "insert heading")

    "p"   '(org-paste-special :wk "org paste special")
    "s"   '(org-insert-structure-template :wk "template")
    "t"   '(org-todo :wk "todo")

    "T"   '(:ignore t :wk "Toggle")
    "Ti"  '(org-toggle-inline-images :wk "inline images")
    "Tl"  '(org-toggle-link-display :wk "link display")
    "Tt"  '(org-toggle-timestamp-type :wk "time-stamp")
    "Tp"  '(org-latex-preview t :wk "preview latex")

    "x"   '(:ignore t :wk "export")
    "xm"  '(org-export-dispatch :wk "dispatch menu")
    "xh"  '(org-html-export-to-html :wk "html")
    "xp"  '(org-latex-export-to-pdf :wk "pdf")
    "xw"  '(org-pandoc-export-to-docs :wk "windows docx")
    "y"   '(org-copy-special :wk "org copy special"))

  (:keymaps 'org-agenda-mode-map
            "j" '(org-agenda-next-line)
            "h" '(org-agenda-previous-line))

  (general-def
    :keymaps 'org-mode-map
    :states 'normal
    "S-TAB" 'org-cycle-global
    "<backtab>" 'org-cycle-global)

  (general-def 'normal org-cdlatex-mode-map
    "(" #'org-cdlatex-pbb
    "[" #'org-cdlatex-pbb
    "{" #'org-cdlatex-pbb)

  :custom
  (org-cite-global-bibliography
   (remq 'nil
         (mapcar
          (lambda (x)
            (let
                ((bibfile
                  (expand-file-name "biblio.bib" x)))
              (if (file-exists-p bibfile) bibfile)))
          pspmacs/ref-paths)))
  ;; edit settings
  (org-special-ctrl-a/e t)
  (org-ellipsis " ↷")
  (org-src-fontify-natively t)
  (org-highlight-latex-and-related '(native))
  (org-startup-with-inline-images nil)
  (org-pretty-entities t)
  (org-return-follows-link t)
  (org-hide-emphasis-markers t)
  (org-roam-dailies-directory pspmacs/org-journal-path)
  (org-startup-folded t)
  (org-todo-keyword-faces pspmacs/hl-tag-faces)

  (org-startup-with-latex-preview t)
  (org-format-latex-options
   (progn (plist-put org-format-latex-options :background "Transparent")
          (plist-put org-format-latex-options :scale 1.5)
          (plist-put org-format-latex-options :zoom 1.0)))

  (org-latex-preview-options
   (progn (plist-put  org-latex-preview-options :background "Transparent")
          (plist-put org-latex-preview-options :scale 1.5)
          (plist-put org-latex-preview-options :zoom 1.0)))
  (org-latex-compiler "xelatex")

  (org-todo-keywords
   '((sequence
      "FAIL(f)"
      "FIXME(m)"
      "TEMP(u)"
      "HACK(h)"
      "TODO(t)"
      "LAZY(l)"
      "WAIT(w)"
      "NEXT(n)"
      "ALGO(g)"
      "PROG(p)"
      "TEST(q)"
      "ACTS(a)"
      "SENT(s)"
      "OKAY(o)"
      "NOTE(n)"
      "XXXX(x)"
      "|"
      "DONE(d)"
      "DONT(!)"
      "CANT©")))

  :config
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((python . t)
     (shell . t)
     (sed . t)
     (emacs-lisp . t)
     (R . t)
     (awk . t)))

  :hook
  ((org-mode . pspmacs/prettify-note)
   (org-mode . visual-line-mode)
   (org-mode . karthink/add-latex-in-org-mode-expansions)))
   ;; (org-mode . turn-on-org-cdlatex)

(use-package org-bullets
  :after org
  :hook (org-mode . (lambda () (org-bullets-mode 1))))

(use-package org-auto-tangle
  :after org
  :hook (org-mode . org-auto-tangle-mode))

(use-package org-roam
  :after org
  :custom
  (org-roam-directory (expand-file-name "roam" pspmacs/org-path)))

(defun karthink/add-latex-in-org-mode-expansions ()
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

(defun karthink/org-export-ignore-headlines (data backend info)
  "Remove headlines tagged \"ignore\" retaining contents and promoting children.
Each headline tagged \"ignore\" will be removed retaining its
contents and promoting any children headlines to the level of the
parent."
  (org-element-map data 'headline
    (lambda (object)
      (when (member "ignore" (org-element-property :tags object))
        (let ((level-top (org-element-property :level object))
              level-diff)
          (mapc (lambda (el)
                  ;; recursively promote all nested headlines
                  (org-element-map el 'headline
                    (lambda (el)
                      (when (equal 'headline (org-element-type el))
                        (unless level-diff
                          (setq level-diff (- (org-element-property :level el)
                                              level-top)))
                        (org-element-put-property el
                                                  :level (- (org-element-property :level el)
                                                            level-diff)))))
                  ;; insert back into parse tree
                  (org-element-insert-before el object))
                (org-element-contents object)))
        (org-element-extract-element object)))
    info nil)
  data)

(setq ox-kw
      (if (string= pspmacs/package-manager 'builtin)
          '(:ensure org)
        '(:straight org)))

(setq ox-kwargs
      `(ox
        ,@ox-kw
        :after org
        :commands org-export-dispatch
        :custom
        (org-html-htmlize-output-type 'css)
        :config
        ;; (add-to-list 'org-latex-packages-alist '("" "listings"))
        ;; (add-to-list 'org-latex-packages-alist '("" "color"))
        (add-hook 'org-export-filter-parse-tree-functions 'karthink/org-export-ignore-headlines)))
(eval `(use-package ,@ox-kwargs))

(setq ox-latex-kwargs
 `(ox-latex
   ,@ox-kw
   :after ox
   :custom
   (org-latex-caption-above nil)
   (org-export-with-LaTeX-fragments t)
   (org-latex-tables-booktabs t)
   (org-export-with-smart-quotes t)
   (org-latex-prefer-user-labels t)
   (org-latex-reference-command "\\cref{%s}")
   ;; From https://git.tecosaur.net/tec/emacs-config, the default link colors
   ;; are hideous.
   (org-latex-hyperref-template
   "\\hypersetup{
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
\\renewcommand{\\tableofcontents}{\\begingroup\\hypersetup{hidelinks}\\oldtoc\\endgroup}")

   :config
   (dolist (package '((""           "longtable" nil)
                      (""           "booktabs"  nil)
                      (""           "color"     nil)
                      (""           "cancel"    t)))
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
             (append (list class-string) extra article-sections))))))
(eval `(use-package ,@ox-latex-kwargs))

(use-package org-ref)

(use-package org-pomodoro
  :after org
  :general
  (pspmacs/leader-keys
    :keymaps 'org-mode-map
    "t"   '(:ignore t :wk "time")
    "tp"  '(:ignore t :wk "pomodoro")
    "tpp" '(org-pomodoro :wk "pomodoro")
    "tpe" '(org-pomodoro-extend-last-clock :wk "extend last")
    "tp?" '((lambda ()
              (interactive)
              (message
               (format-seconds
                "%0.2m:%0.2s left"
                (round (org-pomodoro-remaining-seconds)))))
            :wk "remaining")
    "tpk" '((lambda ()
              (interactive)
              (org-pomodoro-kill))
            :wk "kill")
    "tpx" '((lambda ()
              (interactive)
              (cond
               ((eq org-pomodoro-state :pomodoro)
                (org-pomodoro-finished))
               ((eq org-pomodoro-state :short-break)
                (org-pomodoro-short-break-finished))
               ((eq org-pomodoro-state :long-break)
                (org-pomodoro-long-break-finished))))))
  :custom
  (org-pomodoro-clock-break t)
  (org-pomodoro-manual-break t)
  (org-pomodoro-format "⏰ %s")
  (org-pomodoro-overtime-format "🏃 %s")
  (org-pomodoro-long-break-format "💤 %s")
  (org-pomodoro-short-break-format "⏸ %s")
  (org-pomodoro-long-break-frequency 5)
  (org-pomodoro-short-break-length 5)
  (org-pomodoro-long-break-length 30)
  (org-pomodoro-length 25))

(use-package org-wc
  :after org
  :general
  (pspmacs/local-leader-keys
    :keymaps 'org-mode-map
    "=wt" '(org-wc-display :wk "org word-count tree")))

(use-package org-ai
  :commands (org-ai-mode)
  :general
  (pspmacs/leader-keys
    "Ap"  '(:ignore t :wk "prompt")
    "App" '(org-ai-prompt :wk "prompt")
    "Apm" '(org-ai-mark-last-region :wk "mark")
    "Apr" '(org-ai-on-region :wk "region")

    "As"  '(org-ai-summarize :wk "summarize"))
  :hook
  (org-mode . org-ai-mode)
  :config
  ;; if you are on the gpt-4 beta:
  ;; (setq org-ai-default-chat-model "gpt-4")
  ;; if you are using yasnippet and want `ai` snippets
  (org-ai-install-yasnippets))

(use-package powerthesaurus
  :after org
  :general
  (pspmacs/leader-keys
    "D" '(:ignore t :wk "Dictionary")
    "D=" '(powerthesaurus-lookup-synonyms-dwim :wk "synonym")
    "D+" '(powerthesaurus-lookup-related-dwim :wk "related")
    "D?" '(powerthesaurus-lookup-definitions-dwim :wk "related")
    "D!" '(powerthesaurus-lookup-antonyms-dwim :wk "antonym")
    "DD" '(powerthesaurus-transient :wk "Explore")
    "Dl" '(powerthesaurus-lookup-dwim :wk "lookup")))

(use-package emacs
  :custom
  (diary-file (expand-file-name "diary" xdg/emacs-data-directory)))

(pspmacs/load-inherit)

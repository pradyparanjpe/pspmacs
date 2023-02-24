;;;; pspmacs-note.el --- org-mode -*- lexical-binding: t; -*-

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

(use-package org
  :general
  ;; ORG AGENDA
  (pspmacs/leader-keys
    :keymaps 'org-mode-map
    "a"   '(:ignore t :wk "agenda")
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
    "="   '(:ignore t :wk "count")
    "=w"  '(:ignore t :wk "words")
    "=ww" '(count-words t :wk "all")
    "=wr" '(count-words-region t :wk "region")

    ">"   '(org-demote-subtree :wk "demote subtree")
    "<"   '(org-promote-subtree :wk "demote subtree")

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

    "l"   '(:ignore t :wk "link")
    "ll"  '(org-insert-link t :wk "put")
    "lL"  '(org-store-link t :wk "grab")
    "lp"  '(org-latex-preview t :wk "prev latex")

    "i"   '(:ignore t :wk "insert")
    "ih"  '(org-insert-heading :wk "insert heading")
    "is"  '(org-insert-subheading :wk "insert heading")

    "p"   '(org-paste-special :wk "org paste special")
    "s"   '(org-insert-structure-template :wk "template")
    "t"   '(org-todo :wk "todo")

    "x"   '(:ignore t :wk "export")
    "xm"  '(org-export-dispatch :wk "dispatch menu")
    "xh"  '(org-html-export-to-html :wk "html")
    "xp"  '(org-pandoc-export-to-latex-pdf :wk "pdf")
    "xw"  '(org-pandoc-export-to-docs :wk "windows docx")
    "y"   '(org-copy-special :wk "org copy special"))

  (:keymaps 'org-agenda-mode-map
            "j" '(org-agenda-next-line)
            "h" '(org-agenda-previous-line))

  (general-def
   :states 'normal
   "S-TAB" 'org-cycle-global
   "<backtab>" 'org-cycle-global)

  :custom
  (org-cite-global-bibliography
   `(,(expand-file-name "references.bib" pspmacs/org-path)))
  ;; edit settings
  (org-special-ctrl-a/e t)
  (org-ellipsis " ↷")
  (org-src-fontify-natively t)
  (org-pretty-entities t)
  (org-return-follows-link t)
  (org-hide-emphasis-markers t)
  (org-roam-dailies-directory pspmacs/org-journal-path)
  (org-startup-folded t)
  (org-todo-keyword-faces pspmacs/hl-tag-faces)
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
      "CANT(c)")))

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
   (org-mode . visual-line-mode)))

(use-package org-bullets
  :hook (org-mode . (lambda () (org-bullets-mode 1))))

(use-package org-auto-tangle
  :hook (org-mode . org-auto-tangle-mode))

(use-package org-roam
  :custom
  (org-roam-directory (expand-file-name "roam" pspmacs/org-path)))

(use-package org-pomodoro
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

(use-package org-ref)

(pspmacs/load-inherit)

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

(defvar pspmacs/org-path (expand-file-name "org/" xdg/emacs-data-directory)
  "Org mode base")

(defvar pspmacs/org-template-path
  (expand-file-name "templates" pspmacs/org-path)
  "Org mode templates (setupfile)")

(defvar pspmacs/org-journal-path (expand-file-name "journal" pspmacs/org-path)
  "Journal entries.")

(use-package org
  :init
  (defun pspmacs/prettify-note ()
    (pspmacs/mode-prettify '("lisp" "org")))

  :general
  (pspmacs/local-leader-keys
    :keymaps 'org-mode-map
    "l" '(:ignore t :wk "link")
    "ll" '(org-insert-link t :wk "put")
    "lL" '(org-store-link t :wk "grab")
    "lp" '(org-latex-preview t :wk "prev latex")
    "d" '(org-cut-special :wk "org cut special")
    "y" '(org-copy-special :wk "org copy special")
    "p" '(org-paste-special :wk "org paste special")
    "b" '(:keymap org-babel-map :wk "babel")
    "t" '(org-todo :wk "todo")
    "s" '(org-insert-structure-template :wk "template")
    "e" '(org-edit-special :wk "edit")
    "i" '(:ignore t :wk "insert")
    "ih" '(org-insert-heading :wk "insert heading")
    "is" '(org-insert-subheading :wk "insert heading")
    "f" '(org-footnote-action :wk "footnote action")
    "x" '(:ignore t :wk "export")
    "xm" '(org-export-dispatch :wk "dispatch menu")
    "xh" '(org-html-export-to-html :wk "html")
    "xp" '(org-pandoc-export-to-latex-pdf :wk "pdf")
    "xw" '(org-pandoc-export-to-docs :wk "windows docx")
    ">" '(org-demote-subtree :wk "demote subtree")
    "<" '(org-promote-subtree :wk "demote subtree"))
  (:keymaps 'org-agenda-mode-map
            "j" '(org-agenda-next-line)
            "h" '(org-agenda-previous-line))
  (general-define-key
   :states 'normal
   "S-TAB" 'org-global-cycle)

  :config
  ;; edit settings
  (setq org-special-ctrl-a/e t)
  (setq org-ellipsis " ↷")
  (setq org-src-fontify-natively t)
  (setq org-pretty-entities t)
  (setq org-return-follows-link t)
  (setq org-hide-emphasis-markers t)
  (setq org-re-reveal-single-file t)
  (setq org-roam-dailies-directory pspmacs/org-journal-path)
  (setq org-startup-folded t)

  ;; todo setup
  (setq org-todo-keywords
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

  (setq org-todo-keyword-faces
    '(("FAIL"  .  "#ff3f3f")
      ("FIXME" .  "#ff6f3f")
      ("TEMP"  .  "#ff9f3f")
      ("HACK"  .  "#ffcf3f")
      ("TODO"  .  "#ffff3f")
      ("LAZY"  .  "#e7ff3f")
      ("WAIT"  .  "#cfff3f")
      ("NEXT"  .  "#9fff3f")
      ("ALGO"  .  "#6fff3f")
      ("PROG"  .  "#3fff3f")
      ("TEST"  .  "#3fe757")
      ("ACTS"  .  "#3fcf6f")
      ("SENT"  .  "#3f9f9f")
      ("OKAY"  .  "#3f6fcf")
      ("DONE"  .  "#3f3fff")
      ("NOTE"  .  "#ffcf6f")
      ("XXXX"  .  "#ff9f9f")
      ("DONT"  .  "#ff6fcf")
      ("CANT"  .  "#ff3fff")))
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
  :ensure t
  :hook (org-mode . org-auto-tangle-mode))

(use-package org-roam
  :custom
  (org-roam-directory (expand-file-name "roam" pspmacs/org-path)))

(pspmacs/load-inherit)

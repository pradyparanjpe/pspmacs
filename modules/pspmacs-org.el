;;;; pspmacs-org.el --- org-mode -*- lexical-binding: t; -*-

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
  (defun pspmacs/prettify-org ()
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
  ((org-mode . pspmacs/prettify-org)
   (org-mode . visual-line-mode)))

(use-package org-bullets
:hook (org-mode . (lambda () (org-bullets-mode 1))))

(use-package org-auto-tangle
  :ensure t
  :hook (org-mode . org-auto-tangle-mode))

(defun pspmacs/load-suitable (fname)
  "Load emacs init file FNAME.

If FNAME is found, load it and return.
If org/el counterpart of FNAME is found, load it and return.
To load,
If extension `equal's 'org', use function `org-babel-load-file'.
If extension `equal's 'el', use function `load'"
  (cond
   ((equal (file-name-extension fname) "org")
    (cond ((file-readable-p fname)
       (org-bable-load-file fname))
      ((file-readable-p (file-name-with-extension fname "el"))
       (load (file-name-with-extension fname "el") nil 'nomessage))))
   ((equal (file-name-extension fname) "el")
    (cond ((file-readable-p fname)
       (load fname nil 'nomessage))
      ((file-readable-p (file-name-with-extension fname "org"))
       (org-babel-load-file (file-name-with-extension fname "el")))))))

(defun pspmacs/load-inherit (&optional fname)
  "Inherit all equivalent files.

Re-definition of early-loaded function after the correct orgmode is loaded.
Files may be placed in `pvt-emacs-directory' and/or `local-emacs-directory'.
If FNAME is supplied, *that* corresponding file name is attempted, else,
stem of `load-file-name' is attempted.
Init files are loaded using the function `pspmacs-load-suitable'.
Settings loaded from files located in `pvt-emacs-directory' are overwritten
by settings loaded from files located in `local-emacs-directory'."
  (let ((name-branch
     (file-relative-name (or fname load-file-name) user-emacs-directory)))
    (dolist (config-dir `(,pvt-emacs-directory ,local-emacs-directory) nil)
  (let* ((modular-init (expand-file-name name-branch config-dir)))
    (if (file-exists-p modular-init)
        (pspmacs/load-suitable modular-init))))))

(pspmacs/load-inherit)
(provide 'pspmacs-org)

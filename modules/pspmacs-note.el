;;; pspmacs-note.el --- org-mode -*- lexical-binding: t; -*-

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

    "t"   '(:ignore t :wk "ogl")
    "t="  '(pspmacs/org-toggle-emphasis-display :wk "markers")
    "ti"  '(org-toggle-inline-images :wk "inline images")
    "tl"  '(org-toggle-link-display :wk "link display")
    "tt"  '(org-toggle-timestamp-type :wk "time-stamp")
    "tp"  '(org-latex-preview :wk "preview latex"))

  ;; ORG TABLE
  (pspmacs/local-leader-keys
    :keymaps  'org-mode-map
    "TAB"     '(:ignore t :wk "table")

    "TAB RET" '(org-table-create-or-convert-from-region :wk "create")
    "TAB <"   '(org-table-shrink :wk "shrink")
    "TAB >"   '(org-table-expand :wk "expand")
    "TAB ?"   '(org-table-field-info :wk "field info")

    "TAB P"   '(:ignore t :wk "plot")
    "TAB Pa"  '(orgtbl-ascii-plot :wk "ascii")
    "TAB Pg"  '(org-plot/gnuplot :wk "gnuplot")
    "TAB Pp"  '(org-plot/gnuplot :wk "gnuplot")

    "TAB d"   '(:ignore t :wk "delete")
    "TAB dc"  '(org-table-delete-column :wk "column")
    "TAB dd"  '(org-table-blank-field :wk "field contents")
    "TAB dr"  '(org-table-kill-row :wk "row")

    "TAB i"   '(:ignore t :wk "insert")
    "TAB iH"  '(org-table-hline-and-move :wk "‾‾‾‾")
    "TAB ic"  '(org-table-insert-column :wk "column")
    "TAB ih"  '(org-table-insert-hline :wk "____")
    "TAB ii"  '(table-insert :wk "table")
    "TAB ir"  '(org-table-insert-row :wk "row")

    "TAB p"   '(org-table-paste-rectangle)

    "TAB s"   '(org-table-sort-lines :wk "sort")
    "TAB x"   '(org-table-cut-region :wk "cut")
    "TAB y"   '(org-tablecopy-region))

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
            :wk "all")
    "[!"  '(:ignore t :wk "remove")
    "[!]" '((lambda () (interactive) (pspmacs/org-put-checkboxes t)) :wk "this")
    "[!*" '((lambda () (interactive) (pspmacs/org-put-checkboxes-recursively t))
            :wk "all")

    "S"   '(:ignore t :wk "special")
    "Sx"  '(org-cut-special :wk "org cut special")
    "Se"  '(org-edit-special :wk "edit")

    "b"   '(:keymap org-babel-map :wk "babel")

    "d"   '(:ignore t :wk "date-time")
    "dd"  '(org-deadline :wk "date-time")
    "dT"  '(org-time-stamp-inactive :wk "inactive time stamp")
    "ds"  '(org-schedule :wk "schedule")
    "dt"  '(org-time-stamp :wk "time-stamp")

    "f"   '(org-footnote-action :wk "footnote action")

    "i"   '(:ignore t :wk "insert")
    "ih"  '(org-insert-heading :wk "insert heading")
    "is"  '(org-insert-subheading :wk "insert heading")

    "l"   '(:ignore t :wk "link")
    "lL"  '(org-store-link t :wk "grab")
    "lp"  '(pspmacs/org-paste-as-link :wk "paste")
    "ll"  '(org-insert-link t :wk "put")
    "ly"  '(pspmacs/org-copy-link-at-point :wk "yank")

    "p"   '(org-paste-special :wk "org paste special")
    "s"   '(org-insert-structure-template :wk "template")
    "t"   '(org-todo :wk "todo")

    "x"   '(:ignore t :wk "export")
    "xm"  '(org-export-dispatch :wk "menu")
    "xh"  '(org-html-export-to-html :wk "html")

    "y"   '(org-copy-special :wk "org copy special"))

  (general-def
    :keymaps 'org-agenda-mode-map
    "j" '(org-agenda-next-line)
    "h" '(org-agenda-previous-line))

  :custom
  ;; Base
  (org-directory pspmacs/org-path)
  ;; Org table
  (org-table-automatic-realign nil)
  (org-table-header-line-p t)
  (org-table-shrunk-column-indicator "↷")

  (org-cite-global-bibliography
   (remq 'nil
         (mapcar
          (lambda (x)
            (let ((bibfile (expand-file-name "biblio.bib" x)))
              (if (file-exists-p bibfile) bibfile)))
          pspmacs/ref-paths)))

  ;; Startup display
  (org-startup-with-inline-images t)
  (org-startup-folded t)

  ;; Keybindings
  (org-special-ctrl-a/e t)
  (org-return-follows-link t)

  ;; Images
  (org-image-actual-width nil)

  ;; Prettify
  (org-ellipsis " ↷")
  (org-hide-emphasis-markers t)
  (org-src-fontify-natively t)
  (org-pretty-entities t)
  (org-roam-dailies-directory pspmacs/org-journal-path)
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
  (org-use-sub-super-scripts '{})

  :config
  ;; TeX

  ;; smart-parentheses
  (mapc (lambda (wrap)
          (sp-local-pair 'org-mode wrap wrap
                         :unless '(sp-point-after-word-p)))
        '("=" "~" "/" "$"))
  (sp-local-pair 'org-mode "<" ">"
                 :unless '(sp-in-code-p
                           sp-point-after-word-p
                           sp-point-after-bol-p))
  (sp-local-pair 'org-mode "*" "*"
                 :unless '(pspmacs/at-org-header-p
                           sp-point-after-bol-p
                           sp-point-after-word-p))
  (sp-local-pair 'org-mode "+" "+"
                 :unless '(pspmacs/at-org-in-buffer-settings-p
                           sp-point-after-word-p))
  (sp-local-pair 'org-mode "_" "_"
                 :unless '(pspmacs/at-org-in-buffer-settings-p
                           sp-point-after-word-p))
  (let ((paren-bindings
         (mapcan
          (lambda (wrapper)
            (let ((pair-open wrapper)
                  (pair-close (plist-get (sp-get-pair wrapper) :close)))
              `(,(format "(%s" wrapper)
                 '((lambda (&optional arg)
                     (interactive "P")
                     (sp-wrap-with-pair ,pair-open))
                   :wk ,(format "%s%s" pair-open pair-close)))))
         '("_" "+" "=" "~" "*" "/" "<" "$"))))
    (eval `(pspmacs/leader-keys :keymaps 'org-mode-map ,@paren-bindings)))

  ;; babel source codes
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((awk . t)
     (ditaa . t)
     (emacs-lisp . t)
     (latex . t)
     (lisp . t)
     (python . t)
     (R . t)
     (shell . t)
     (sed . t)))

  :hook
  ((org-mode . pspmacs/prettify-note)
   (org-mode . visual-line-mode)))
  ;; (org-mode . turn-on-org-cdlatex)

(use-package org-auto-tangle
  :after org
  :hook (org-mode . org-auto-tangle-mode))

(use-package org-roam
  :after org
  :custom
  (org-roam-directory (expand-file-name "roam" pspmacs/org-path)))

(defun karthink/org-export-ignore-headlines (data backend info)
  "Remove headlines tagged \"ignore\" retaining contents and promoting children.
Each headline tagged \"ignore\" will be removed retaining its
contents and promoting any children headlines to the level of the
parent."
  (org-element-map
      data
      'headline
    (lambda (object)
      (when (member "ignore" (org-element-property :tags object))
        (let ((level-top (org-element-property :level object)) level-diff)
          (mapc (lambda (el)
                  ;; recursively promote all nested headlines
                  (org-element-map
                      el
                      'headline
                    (lambda (el)
                      (when (equal 'headline (org-element-type el))
                        (unless level-diff
                          (setq
                           level-diff
                           (- (org-element-property :level el) level-top)))
                        (org-element-put-property
                         el
                         :level
                         (- (org-element-property :level el) level-diff)))))
                  ;; insert back into parse tree
                  (org-element-insert-before el object))
                (org-element-contents object)))
        (org-element-extract-element object)))
    info nil)
  data)

(use-package ox
  :ensure org
  :after org
  :commands org-export-dispatch
  :custom
  (org-html-htmlize-output-type 'css)
  (org-html-metadata-timestamp-format "%a, %Y-%m-%d %H:%M%z")
  (org-time-stamp-custom-formats '("<%a, %Y-%m-%d>" . "<%a, %Y-%m-%d %H:%M%z>"))
  :config
  ;; (add-to-list 'org-latex-packages-alist '("" "listings"))
  ;; (add-to-list 'org-latex-packages-alist '("" "color"))
  (add-hook 'org-export-filter-parse-tree-functions
            'karthink/org-export-ignore-headlines))

(use-package ox-odt
  :after org
  :vc (ox-odt :url "https://github.com/kjambunathan/org-mode-ox-odt")
  :general
  (pspmacs/local-leader-keys
    :keymaps 'org-mode-map
    "xo" '(org-odt-export-to-odt :wk "odt")
    "xw"  '(org-pandoc-export-to-docs :wk "MSdocx"))

  :custom
  ;; (org-odt-preferred-output-format "docx")
  (org-odt-transform-processes
   '(("Optimize Column Width of all Tables"
      "soffice" "--norestore" "--invisible" "--headless"
      "macro:///OrgMode.Utilities.OptimizeColumnWidth(%I)")
     ("Update All"
      "soffice" "--norestore" "--invisible" "--headless"
      "macro:///OrgMode.Utilities.UpdateAll(%I)")
     ("Reload"
      "soffice" "--norestore" "--invisible" "--headless"
      "macro:///OrgMode.Utilities.Reload(%I)")))

  :config
  (add-to-list 'auto-mode-alist
               '("\\.\\(?:OD[CFIGPST]\\|od[cfigpst]\\)\\'"
                 . doc-view-mode-maybe))
  (setcdr (assq 'system org-file-apps-gnu) "xdg-open %s")
  (advice-add 'org-open-file :around
              (lambda (orig-fun &rest args)
                ;; Work around a weird problem with xdg-open.
                (let ((process-connection-type nil))
                  (apply orig-fun args)))))

(use-package ox-gfm)

(use-package org-present
  :general
  (pspmacs/local-leader-keys :keymaps 'org-mode-map
    "P" '(:ignore t :wk "Present")
    "PP" '(org-present :wk "here")
    "P0" '((lambda () (interactive)
             (beginning-of-buffer)
             (org-present))
           :wk "afresh"))

  (pspmacs/local-leader-keys :keymaps 'org-present-mode-keymap
    "q" '(org-present-quit :wk "Quit")
    "]" '(hydra-curtains/body :wk "Widen"))

  :custom
  (org-present-text-scale 3)
  (pspmacs/present-settings
   '((visual-line-mode . 1)
     (display-line-numbers-mode . -1)
     (visual-fill-column-center-text . t)
     (visual-fill-column-width . 80)
     (visual-fill-column-mode . 1)
     (buffer-read-only . 1)
     (header-line-format . "")  ;; blank head-space place holder
     (mode-line-format . "")  ;; blank foot-space place holder
     ((lambda ()) . org-cycle-set-startup-visibility)
     ;; display images
     (org-display-inline-images
      . (lambda () (interactive)
          (unless org-startup-with-inline-images (org-remove-inline-images))))
     ;; fullscreen
     ((lambda () (interactive)
        (defvar pspmacs/frame-fullscreen-was (frame-parameter nil 'fullscreen))
        (set-frame-parameter nil 'fullscreen 'fullboth))
      . (lambda () (interactive)
          (set-frame-parameter nil 'fullscreen pspmacs/frame-fullscreen-was)
          (makunbound 'pspmacs/frame-fullscreen-was)))
     ;; Large faces
     (face-remapping-alist
      . '((default (:height 1.5) variable-pitch)
          (header-line (:height 4.0) variable-pitch)
          (org-document-title (:height 1.75) org-document-title)
          (org-code (:height 1.55) org-code)
          (org-verbatim (:height 1.55) org-verbatim)
          (org-block (:height 1.25) org-block)
          (org-block-begin-line (:height 0.7) org-block)))))
  :config
  (require 'visual-fill-column)
  (defhydra hydra-curtains ()
    "Widen View"
    ("]" (lambda () (interactive)
           (setq-local visual-fill-column-width
                       (+ visual-fill-column-width 1)))
     "wide")
    ("[" (lambda () (interactive)
           (setq-local visual-fill-column-width
                       (- visual-fill-column-width 1)))
     "narrow"))
  (add-hook 'org-present-after-navigate-functions #'pspmacs/next-slide)
  :hook
  (org-present-mode . pspmacs/present-start)
  (org-present-mode-quit . pspmacs/present-end))

(customize-set-variable 'oer-reveal-org-includes-dir
                        (expand-file-name "oer-reveal-org" local-emacs-dir))
(use-package emacs-reveal
  :ensure nil
  :vc (emacs-reveal :url "https://gitlab.com/oer/emacs-reveal")
  :general
  (pspmacs/local-leader-keys
    :keymaps 'org-mode-map
    "xv" '(org-pandoc-export-to-revealjs :wk "reveal"))
  :custom
  (oer-reveal-org-includes-dir
   (expand-file-name "oer-reveal-org" local-emacs-dir))
  (org-re-reveal-single-file t)
  :hook (org-mode . reveal-mode))

(use-package org-pomodoro
  :after org
  :general
  (pspmacs/leader-keys
    :keymaps 'org-mode-map
    "T"   '(:ignore t :wk "ime")
    "Tc"  '(:ignore t :wk "clock")
    "Tcc" '(org-clock-cancel :wk "cancel")
    "Tci" '(org-clock-in :wk "in")
    "Tco" '(org-clock-out :wk "out")
    "Tcj" '(org-clock-goto :wk "goto")

    "Tp"  '(:ignore t :wk "pomodoro")
    "Tpp" '(org-pomodoro :wk "pomodoro")
    "Tpe" '(org-pomodoro-extend-last-clock :wk "extend last")
    "Tp?" '((lambda ()
              (interactive)
              (message
               (format-seconds
                "%0.2m:%0.2s left"
                (round (org-pomodoro-remaining-seconds)))))
            :wk "remaining")
    "Tpk" '((lambda ()
              (interactive)
              (org-pomodoro-kill))
            :wk "kill")
    "Tpx" '((lambda ()
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

(use-package powerthesaurus
  :after org
  :general
  (pspmacs/leader-keys
    "D!" '(powerthesaurus-lookup-antonyms-dwim :wk "antonym")
    "D+" '(powerthesaurus-lookup-related-dwim :wk "related")
    "D=" '(powerthesaurus-lookup-synonyms-dwim :wk "synonym")
    "D?" '(powerthesaurus-lookup-definitions-dwim :wk "define")
    "DL" '(powerthesaurus-transient :wk "explore")
    "Dl" '(powerthesaurus-lookup-dwim :wk "lookup")))

(use-package org-modern
  :custom
  (org-modern-todo-faces
   '(("FAIL" :foreground "#ff3f3f")
     ("FIXME" :foreground "#ff6f3f")
     ("TEMP" :foreground "#ff9f3f")
     ("HACK" :foreground "#ffcf3f")
     ("TODO" :foreground "#ffff3f")
     ("LAZY" :foreground "#e7ff3f")
     ("WAIT" :foreground "#cfff3f")
     ("NEXT" :foreground "#9fff3f")
     ("ALGO" :foreground "#6fff3f")
     ("PROG" :foreground "#3fff3f")
     ("TEST" :foreground "#3fe757")
     ("ACTS" :foreground "#3fcf6f")
     ("SENT" :foreground "#3f9f9f")
     ("OKAY" :foreground "#3f6fcf")
     ("DONE" :foreground "#3f3fff")
     ("NOTE" :foreground "#ffcf6f")
     ("XXXX" :foreground "#ff9f9f")
     ("DONT" :foreground "#ff6fcf")
     ("CANT" :foreground "#ff3fff")))
  (org-modern-block-name
   '(("note"    "📋" "⏎")
     ("quote"   "🗣"   "🙊")
     ("example" "🥚" "⏎")
     ("src"     "🤖" "⏎")
     ("tip"     "💡" "👍")
     ("warn"    "⚠" "⏎")
     ("warning" "⚠" "⏎")
     ("danger"  "🕱" "⏎")))
  (org-modern-keyword
   '(("setupfile"   . "🛒")
     ("author"      . "🖋")
     ("email"       . "✉")
     ("language"    . "🗣")
     ("options"     . "🔘")
     ("property"    . "⚙")
     ("results"     . "📜")
     ("startup"     . "")
     ("html_head"   . "")
     ("attr_latex:" . "🖺")
     ("title"       . "§")
     ("auto_tangle" . "🤖🔗")
     ("html"        . "")
     (t . "≡")))
  (org-modern-checkbox
   '((88 . "✔")
     (45 . "⏳")
     (32 . "❌")))
  :hook
  (org-mode . org-modern-mode)
  (org-agenda-finalize . org-modern-agenda))

(use-package valign
  :after org-modern
  :custom
  (org-modern-table nil)
  :hook
  (org-modern-mode . valign-mode))

(use-package org-capture
  :ensure org
  :general
  (pspmacs/leader-keys
    "o" '(:ignore t :wk "rg")
    "oc" '(org-capture :wk "capture"))
  :init
  (unless (file-exists-p (expand-file-name "tasks.org" pspmacs/org-path))
    (write-region "* Misc\n" nil (expand-file-name "tasks.org" pspmacs/org-path)))
  :custom
  (org-capture-templates
   `(("t" "Tasks")
     ("tt" "Task"
      entry (file+olp ,(expand-file-name "tasks.org" pspmacs/org-path) "Misc")
      "* TODO %?\n  %U\n  %a\n  %i" :empty-lines 1))))

(use-package org-dial
  :after org
  :demand t
  :vc (org-dial :url "https://github.com/mistrey/org-dial"))

(use-package visual-fill-column
  :custom
  (visual-fill-column-center-text t))

(abbrev-table-put org-mode-abbrev-table
                  :regexp "\\(?:^\\|[\t\s]+\\)\\(?1:,?.*\\)")

(use-package emacs
  :custom
  (diary-file (xdg/make-path "diary" 'cache)))

(pspmacs/load-inherit)

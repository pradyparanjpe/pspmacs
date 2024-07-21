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
    "ao"  '(:ignore t :wk "rg-agenda")
    "ao#" '(org-agenda-list-stuck-projects :wk ":Stuck")
    "ao/" '(org-occur-in-agenda-files :wk ":Occur in agenda")
    "aoa" '(org-agenda-list :wk ":List")
    "aoc" '(org-agenda-capture :wk "apture")
    "aoo" '(org-agenda :wk ":Capture")
    "aot" '(org-todo-list :wk "odo")

    "t"   '(:ignore t :wk "ogl")
    "t="  '(pspmacs/org-toggle-emphasis-display :wk ":Markers")
    "ti"  '(org-toggle-inline-images :wk "nline images")
    "tl"  '(org-toggle-link-display :wk "ink display")
    "tt"  '(org-toggle-timestamp-type :wk "ime-stamp")
    "tp"  '(org-latex-preview :wk "review latex"))

  ;; ORG TABLE
  (pspmacs/local-leader-keys
    :keymaps  'org-mode-map
    "TAB"     '(:ignore t :wk ":Table")

    "TAB RET" '(org-table-create-or-convert-from-region :wk ":Create")
    "TAB <"   '(org-table-shrink :wk ":Shrink")
    "TAB >"   '(org-table-expand :wk ":Expand")
    "TAB ?"   '(org-table-field-info :wk ":Field info")

    "TAB P"   '(:ignore t :wk "lot")
    "TAB Pa"  '(orgtbl-ascii-plot :wk "scii")
    "TAB Pg"  '(org-plot/gnuplot :wk "nuplot")

    "TAB d"   '(:ignore t :wk "elete")
    "TAB dc"  '(org-table-delete-column :wk "olumn")
    "TAB dd"  '(org-table-blank-field :wk ":Field contents")
    "TAB dr"  '(org-table-kill-row :wk "ow")

    "TAB i"   '(:ignore t :wk "nsert")
    "TAB iH"  '(org-table-hline-and-move :wk "‾‾‾‾")
    "TAB ic"  '(org-table-insert-column :wk "olumn")
    "TAB ih"  '(org-table-insert-hline :wk "____")
    "TAB ii"  '(table-insert :wk ":Table")
    "TAB ir"  '(org-table-insert-row :wk "ow")

    "TAB p"   '(org-table-paste-rectangle :wk "aste rect")

    "TAB s"   '(org-table-sort-lines :wk "ort")
    "TAB x"   '(org-table-cut-region :wk ":Cut")
    "TAB y"   '(org-table-copy-region :wk "ank"))

  (pspmacs/local-leader-keys
    :keymaps 'org-mode-map
    "@"   '(:ignore t :wk ":Reference")

    "="   '(:ignore t :wk ":Count")
    "=w"  '(:ignore t :wk "ords")
    "=ww" '(count-words t :wk ":All")
    "=wr" '(count-words-region t :wk "egion")

    ">"   '(org-demote-subtree :wk ":Demote subtree")
    "<"   '(org-promote-subtree :wk ":Promote subtree")

    "["   '(:ignore t :wk "X]boxes")
    "[]"  '(pspmacs/org-put-checkboxes :wk ":Here")
    "[*"  '(pspmacs/org-put-checkboxes-recursively :wk ":All")
    "[!"  '(:ignore t :wk ":Remove")
    "[!]" '((lambda () (interactive) (pspmacs/org-put-checkboxes t))
            :wk ":This")
    "[!*" '((lambda ()
              (interactive)
              (pspmacs/org-put-checkboxes-recursively t))
            :wk ":All")

    "S"   '(:ignore t :wk "pecial")
    "Sx"  '(org-cut-special :wk ":Cut")
    "Se"  '(org-edit-special :wk "dit")

    "b"   '(:keymap org-babel-map :wk "abel")

    "d"   '(:ignore t :wk "ate-time")
    "dd"  '(org-deadline :wk "eadline")
    "dT"  '(org-time-stamp-inactive :wk "ime-stamp inactive")
    "ds"  '(org-schedule :wk "chedule")
    "dt"  '(org-time-stamp :wk "ime-stamp")

    "f"   '(org-footnote-action :wk "ootnote action")

    "i"   '(:ignore t :wk "nsert")
    "ih"  '(org-insert-heading :wk "eading")
    "is"  '(org-insert-subheading :wk "ub-heading")

    "l"   '(:ignore t :wk "ink")
    "lL"  '(org-store-link :wk ":Grab")
    "lp"  '(pspmacs/org-paste-as-link :wk "aste")
    "ll"  '(org-insert-link :wk ":Put")
    "ly"  '(pspmacs/org-copy-link-at-point :wk "ank")

    "p"   '(org-paste-special :wk "aste special")
    "s"   '(org-insert-structure-template :wk ":Template")
    "t"   '(org-todo :wk "odo")

    "x"   '(:ignore t :wk "port")
    "xm"  '(org-export-dispatch :wk "enu")
    "xh"  '(org-html-export-to-html :wk "tml")

    "y"   '(org-copy-special :wk "ank special"))

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
    "xo" '(org-odt-export-to-odt :wk "dt")
    "xw"  '(org-pandoc-export-to-docs :wk ":MSdocx"))

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
    "P" '(:ignore t :wk "resent")
    "PP" '(org-present :wk ":Here")
    "P0" '((lambda () (interactive)
             (beginning-of-buffer)
             (org-present))
           :wk ":Afresh"))

  (pspmacs/local-leader-keys :keymaps 'org-present-mode-keymap
    "q" '(org-present-quit :wk "uit")
    "]" '(hydra-curtains/body :wk ":Widen"))

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
    "xv" '(org-pandoc-export-to-revealjs :wk ":Reveal"))
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
    "Tc"  '(:ignore t :wk "lock")
    "Tcx" '(org-clock-cancel :wk ":Cancel")
    "Tci" '(org-clock-in :wk "n")
    "Tco" '(org-clock-out :wk "ut")
    "Tcj" '(org-clock-goto :wk ":Goto")

    "Tp"  '(:ignore t :wk "omodoro")
    "Tpp" '(org-pomodoro :wk "omodoro")
    "Tpe" '(org-pomodoro-extend-last-clock :wk "xtend last")
    "Tp?" '((lambda ()
              (interactive)
              (message
               (format-seconds
                "%0.2m:%0.2s left"
                (round (org-pomodoro-remaining-seconds)))))
            :wk ":Remaining")
    "Tpk" '((lambda ()
              (interactive)
              (org-pomodoro-kill))
            :wk "ill")
    "Tpx" '((lambda ()
              (interactive)
              (cond
               ((eq org-pomodoro-state :pomodoro)
                (org-pomodoro-finished))
               ((eq org-pomodoro-state :short-break)
                (org-pomodoro-short-break-finished))
               ((eq org-pomodoro-state :long-break)
                (org-pomodoro-long-break-finished))))
            :wk ":Finish"))
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
    "=wt" '(org-wc-display :wk "ree")))

(use-package powerthesaurus
  :after org
  :general
  (pspmacs/leader-keys
    "D!" '(powerthesaurus-lookup-antonyms-dwim :wk ":Antonym")
    "D+" '(powerthesaurus-lookup-related-dwim :wk ":Related")
    "D=" '(powerthesaurus-lookup-synonyms-dwim :wk ":Synonym")
    "D?" '(powerthesaurus-lookup-definitions-dwim :wk ":Define")
    "DL" '(powerthesaurus-transient :wk ":Explore")
    "Dl" '(powerthesaurus-lookup-dwim :wk "ookup")))

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
    "oc" '(org-capture :wk "apture"))
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

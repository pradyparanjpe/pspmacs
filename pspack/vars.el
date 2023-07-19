;;; pspack/vars.el --- common pspmacs variables -*- lexical-binding: t; -*-
;;; Commentary:
;;
;; Custom variables for pspmacs config.
;;
;;; Code:

(require 'pspmacs/xdg)

(defcustom pspmacs/hl-tag-faces
  '(("FAIL"  . "#ff3f3f")
    ("FIXME" . "#ff6f3f")
    ("TEMP"  . "#ff9f3f")
    ("HACK"  . "#ffcf3f")
    ("TODO"  . "#ffff3f")
    ("LAZY"  . "#e7ff3f")
    ("WAIT"  . "#cfff3f")
    ("NEXT"  . "#9fff3f")
    ("ALGO"  . "#6fff3f")
    ("PROG"  . "#3fff3f")
    ("TEST"  . "#3fe757")
    ("ACTS"  . "#3fcf6f")
    ("SENT"  . "#3f9f9f")
    ("OKAY"  . "#3f6fcf")
    ("DONE"  . "#3f3fff")
    ("NOTE"  . "#ffcf6f")
    ("XXXX"  . "#ff9f9f")
    ("DONT"  . "#ff6fcf")
    ("CANT"  . "#ff3fff"))
  "Highlight colors for TODO tags."
  :group 'pspack
  :type '(repeat (cons (string :tag "hl-tag") (string :tag "color"))))

(defcustom pspmacs/pretty-alist
  '(("code" . (("\\n" . ?⏎)
               ("\\t" . ?↹)))
    ("lisp" . (("lambda" . ?λ)))
    ("org" . (("#+setupfile" . ?🛒)
              ("#+author" . ?🖋)
              ("#+begin_note" . ?📋)
              ("#+end_note" . ?⏎)
              ("#+begin_example" . ?🥚)
              ("#+end_example" . ?⏎)
              ("#+begin_src" . ?🤖)
              ("#+end_src" . ?⏎)
              ("#+begin_tip" . ?💡)
              ("#+end_tip" . ?👍)
              ("#+begin_warn" . ?⚠)
              ("#+end_warn" . ?⏎)
              ("#+begin_warning" . ?⚠)
              ("#+end_warning" . ?⏎)
              ("#+email" . ?✉)
              ("#+language" . ?🗣)
              ("#+options" . ?🔘)
              ("#+property" . ?⚙)
              ("#+results" . ?📜)
              ("#+startup" . ?)
              ("#+html_head" . ?)
              ("#+attr_latex:" . ?🖺)
              ("#+title" . ?§)
              (":properties:" . ?)
              (":end:" . ?⏎)
              ("tangle" . ?🔗)
              ("shebang" . ?⌘)
              ("[x]" . ?✔)
              ("[ ]" . ?❌)
              ("[-]" . ?⏳)))
    ("python" . (("->" . ?⇒)))
    ("rust" . ()))
  "Pretty symbols."
  :group 'pspack
  :type '(repeat (cons
                  (string :tag "major-mode")
                  (repeat (cons (string :tag "to prettify")
                                (integer :tag "Pretty symbol ORD"))))))

(defface pspmacs/r-namespace-face '((t (:foreground "#9f7fff")))
  "R package namespace."
  :group 'pspack)

(defface pspmacs/r-name-obj-face '((t (:foreground "#7f97af")))
  "Object referred from R package namespace."
  :group 'pspack)

(defface pspmacs/r-list-face '((t (:foreground "#bf8faf")))
  "R list."
  :group 'pspack)

(defface pspmacs/r-list-obj-face '((t '(:foreground "#9fb7cf")))
  "Object referred from R list"
  :group 'pspack)

(defcustom pspmacs/r-keywords
  '(("\\W\\(\\(\\s_\\|\\w\\|\\.\\)+\\)::"
     1 'pspmacs/r-namespace-face prepend)
    ("\\w::\\(\\(\\s_\\|\\w\\|\\.\\)+\\)"
     1 'pspmacs/r-name-obj-face prepend)
    ("\\(\\(\\s_\\|\\w\\|\\.\\)+\\)\\$\\w"
     1 'pspmacs/r-list-face prepend)
    ("\\w\\$\\(\\(\\s_\\|\\w\\|\\.\\)+\\)"
       1 'pspmacs/r-list-obj-face prepend))
  "Custom keywords to highlight in R mode"
  :group 'pspack
  :type '(repeat (list :tag "R highlight keywords")))

(defface pspmacs/pyargs-face
  '((t (:foreground "#9f7fff")))
  "Python arguments face identified as '*args' and '**kwargs'."
  :group 'pspack)

(defface pspmacs/pyprivate-face
  '((t (:slant italic :box t)))
  "python private symbols identified as '_private'."
  :group 'pspack)

(defface pspmacs/pydunder-face
  '((t (:slant italic :foreground "#cfff40")))
  "python dunder symbols identified as '__dunder__'."
  :group 'pspack)

(defface pspmacs/rst-literal-face
  '((t (:box t)))
  "Restructured text literals delimited by double backquotes `\`\`True\`\``."
  :group 'pspack)

(defcustom pspmacs/py-keywords
  '(("\\W\\(\\*\\{1,2\\}\\(\\s_\\|\\w\\|\\.\\)+\\)"
     1 'pspmacs/pyargs-face t append)
    ("\\W\\(_\\{1,2\\}\\(\\s_\\|\\w\\|\\.\\)+_\\{0,2\\}\\)"
     1 'pspmacs/pyprivate-face prepend)
    ("\\W\\(__\\(\\s_\\|\\w\\|\\.\\)+__\\)"
     1 'pspmacs/pydunder-face t)
    ("\\W\\(\\([0-9]*_?[0-9]+\\)+\\(\\.[0-9]*\\)?\\)"
     1 'font-lock-constant-face nil)
    ("\\W\\(\\([0-9]*_?[0-9]+\\)*\\(\\.[0-9]+\\)\\)"
     1 'font-lock-constant-face nil)
    (") ?\\(->\\) ?" 1 'font-lock-keyword-face nil)
    ("``\\(.*?\\)``" 1 'pspmacs/rst-literal-face prepend))
  "Custom keywords to highlight in python mode"
  :group 'pspack
  :type '(repeat (list :tag "Python highlight keywords")))

(defcustom pspmacs/elisp-keywords
  '(("\\W\\(\\([0-9]*_?[0-9]+\\)*\\(\\.[0-9]+\\)\\)"
     1 'font-lock-constant-face nil)
    ("\\W\\(t\\|\\nil)\\W"
     1 'font-lock-constant-face nil))
  "Custom keywords to highlight in emacs-lisp mode"
  :group 'pspack
  :type '(repeat (list :tag "emacs-lisp highlight keywords")))

(defcustom pspmacs/font-height 150
  "10 x Font-height"
  :group 'pspack
  :type 'integer)

(use-package yaml
  ;; This is early configuration
  ;; Further configuration is maintained
  ;; under modular tree
  :ensure t
  :demand t)
(use-package ht
  :demand t)
(use-package f
  :demand t)

(defcustom pspmacs/modules-order
  (let
      ((modules-dir
        (mapcar
         (lambda (x) (expand-file-name "modules" x)) pspmacs/worktrees)))
    (apply
     'vconcat (mapcar
               (lambda (x) (cdr x))
               (sort
                (ht->alist
                (apply
                 'ht-merge
                 (remq 'nil
                       (mapcar
                        (lambda (x)
                          (let
                              ((order-file
                                (expand-file-name "load-order.yml" x)))
                            (if (file-readable-p order-file)
                                (yaml-parse-string
                                 (f-read-text order-file)))))
                        modules-dir))))
               (lambda (a b) (< (car a) (car b)))))))
  "Ordered list of pspmacs/modules to load."
  :group 'pspack
  :type '(repeat (string :tag "module-name")))

(defcustom pspmacs/byte-worktree t
  "Byte compile worktrees?"
  :group 'pspack
  :type 'boolean)

(defcustom pspmacs/mode-keybindings
  '((fundamental-mode . "-")
    (conf-mode . "cc")
    (js-json-mode . "cj")
    (markdown-mode . "md")
    (lisp-interaction-mode . "i")
    (rst-mode . "mr")
    (toml-mode . "mt")
    (TeX-mode . "mX")
    (xml-mode . "mx")
    (yaml-mode . "my")
    (org-mode . "o")
    (c-mode . "pc")
    (c++-mode . "pC")
    (emacs-lisp-mode . "pe")
    (java-mode . "pj")
    (lua-mode . "pl")
    (python-mode . "pp")
    (rust-mode . "pr")
    (ess-r-mode . "pR")
    (shell-script-mode . "ps")
    (ruby-mode . "py")
    (html-mode . "wh")
    (javasript-mode . "wj")
    (css-mode . "wc"))
  "Common keybindings for buffer major modes"
  :group 'pspmacs
  :type '(repeat (cons (symbol :tag "mode")
                       (string :tag "key-sequence"))))

(require 'pspmacs/xdg)
(defcustom pspmacs/org-path
  (expand-file-name "org/" xdg/emacs-data-directory)
  "Org mode base"
  :group 'pspmacs
  :type '(string :tag "Org files base"))

(defcustom pspmacs/org-template-path
  (expand-file-name "templates" pspmacs/org-path)
  "Org mode templates (setupfile)"
  :group 'pspmacs
  :type '(string :tag "Org templates"))

(defcustom pspmacs/org-journal-path
  (expand-file-name "journal" pspmacs/org-path)
  "Journal entries."
  :group 'pspmacs
  :type '(string :tag "Org Journal"))

(defcustom pspmacs/ref-paths
  `(,(expand-file-name "references/" xdg/emacs-data-directory))
  "Reference base paths"
  :group 'pspmacs
  :type '(list (string :tag "Base to references")))

(defcustom pspmacs/set-mailbox nil
  "Set Emacs Mailbox (Mu4e, mbsync)"
  :group 'pspmacs
  :type 'boolean)

(defcustom pspmacs/after-code-load-hook nil
  "run after the program code file is loaded"
  :group 'pspack
  :type '(hook :tag "After code-load"))

(defcustom pspmacs/project-init-hook nil
  "Hook called to initialize project"
  :group 'pspack
  :type '(hook :tag "Initialize project"))

(defcustom pspmacs/project-init-command nil
  "Project initialization command"
  :group 'pspack
  :type '(hook :tag "Initialize project"))

(defcustom pspmacs/serve-or-run-hook nil
  "hook called to initialize project"
  :group 'pspack
  :type '(hook :tag "Run or serve project"))

(defcustom pspmacs/serve-or-run-command nil
  "Project initialization command"
  :group 'pspack
  :type '(hook :tag "Serve or run project"))

;;; vars.el ends there

;;; pspack/vars.el --- common pspmacs variables -*- lexical-binding: t; -*-
;;; Commentary:
;;
;; Custom variables for pspmacs config.
;;
;;; Code:

(defcustom pspmacs/home-splash-before-hook nil
  "Run before switching to pspmacs/home-splash."
  :group 'pspack
  :type '(hook :tag "Before home-splash"))

(defcustom pspmacs/home-splash-hook nil
  "Run after switching to pspmacs/home-splash."
  :group 'pspack
  :type '(hook :tag "After home-splash"))

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
               ("\\t" . ?↹)
               (">=" . ?≥)
               ("<=" . ?≤)
               ("!=" . ?≠)
               ("==" . ?≅)))
    ("lisp" . (("lambda" . ?λ)))
    ("org" . (("#+setupfile" . ?🛒)
              ("#+author" . ?🖋)
              ("#+begin_note" . ?📋)
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
              ("#+title" . ?§)
              ("tangle" . ?🔗)
              ("[x]" . ?✔)
              ("[ ]" . ?❌)
              ("[-]" . ?⏳)))
    ("python" . (("and" . ?∩)
                 ("or" . ?∪)
                 ("->" . ?⇒))))
  "Pretty symbols."
  :group 'pspack
  :type '(repeat (cons
                  (string :tag "major-mode")
                  (repeat (cons (string :tag "to prettify")
                                (integer :tag "Pretty symbol ORD"))))))

(defface pspmacs/r-namespace '((t (:foreground "#9f7fff")))
  "R package namespace."
  :group 'pspack)

(defface pspmacs/r-name-obj '((t (:foreground "#8fa7bf")))
  "Object referred from R package namespace."
  :group 'pspack)

(defface pspmacs/r-list '((t (:foreground "#bf8faf")))
  "R list."
  :group 'pspack)

(defface pspmacs/r-list-obj '((t '(:foreground "#8fa7bf")))
  "Object referred from R list"
  :group 'pspack)

(defface pspmacs/pyargs '((t (:foreground "#9f7fff")))
  "Python arguments face identified as '*args' and '**kwargs'."
  :group 'pspack)

(defface pspmacs/pydunder '((t (:foreground "#bf8fa7")))
  "python dunder symbols identified as '__dunder__'."
  :group 'pspack)

(defface pspmacs/rst-literal '((t (:box t)))
  "Restructured text literals delimited by double backquotes `\`\`True\`\``."
  :group 'pspack)

(defun pspmacs/prettify-python ()
  "Prettify python"
  (pspmacs/mode-prettify '("code" "python")))

(use-package yaml)
(use-package ht)
(use-package f)

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

;;; vars.el ends there

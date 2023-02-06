;;; late/vars.el --- common pspmacs varstions -*- lexical-binding: t; -*-
;;; Commentary:
;;
;; Custom variables for pspmacs config.
;;
;;; Code:

(defvar pspmacs/home-splash-before-hook nil
  "Run before switching to pspmacs/home-splash.")

(defvar pspmacs/home-splash-hook nil
  "Run after switching to pspmacs/home-splash.")

(defcustom pspmacs/hl-tag-faces
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
    ("CANT"  .  "#ff3fff"))
  "Highlight colors for TODO tags."
  :group 'pspmacs)

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
              ("#+begin_src" . ?)
              ("#+end_src" . ?⏎)
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
  :group 'pspmacs)

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
  :group 'pspmacs)

(pspmacs/load-inherit)
;;; vars.el ends here

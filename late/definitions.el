;;; late/definitions.el --- identifies platform -*- lexical-binding: t; -*-

(load (expand-file-name "late/version.el" user-emacs-directory) nil 'nomessage)

(defgroup pspmacs nil
  "PSPMacs Custom init configuration for Emacs"
  :version pspmacs/version)

(defconst ON-LINUX   (eq system-type 'gnu/linux))
(defconst ON-MAC     (eq system-type 'darwin))
(defconst ON-BSD     (or ON-MAC (eq system-type 'berkeley-unix)))
(defconst ON-WINDOWS (memq system-type '(cygwin windows-nt ms-dos)))

(defun pspmacs/inferior-interpreter (executable)
  "Open an inferior interpreter in split window

Open EXECUTABLE interpreter in an inferior windows."
  (interactive)
  (let ((interpreter-window (split-window-below)))
    (select-window interpreter-window)
    (call-interactively executable)))

(defun pspmacs/destroy-buffer-and-window (&optional target-buffer)
  "Destroy window and buffer after some process is done

If TARGET-BUFFER is supplied, it and its window is destroyed.
Else, current buffer and window is destroyed.
If window is the only window, it is spared"
  (let* ((used-buffer (or target-buffer (current-buffer)))
         (used-window (get-buffer-window used-buffer)))
    (when (not (one-window-p))
      (delete-window used-window))
    (kill-buffer used-buffer)))

(defun pspmacs/kill-other-buffers ()
  "Kill all other buffers."
  (interactive)
  (mapc 'kill-buffer (delq (current-buffer) (buffer-list))))

(defun pspmacs/extend-list (list-var elements)
  "Iterative form of ‘add-to-list’.

Return value is the new value of LIST-VAR"
  (unless (consp elements)
    (error "ELEMENTS must be list"))
  (dolist (elem elements)
    (add-to-list list-var elem))
  (symbol-value list-var))

(defun pspmacs/mode-prettify (sub-modes)
  "Apply pretiffy mode alist according to active-mode"
  (progn
    (setq
     prettify-symbols-alist
     (mapcan (lambda (x)
               (list x `(,(upcase (car x)) . ,(cdr x))))
             (apply #'append
                    (mapcar
                     (lambda (y)
                       (cdr (assoc y pspmacs/pretty-alist)))
                     sub-modes))))
    (prettify-symbols-mode)))

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
  "Highlight colors for TODO tags"
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
  "pretty symbols"
  :group 'pspmacs)

(load (expand-file-name "late/xdg.el" user-emacs-directory) nil 'nomessage)

(pspmacs/load-inherit)

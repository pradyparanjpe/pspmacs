;;; pspack/func.el --- common pspmacs functions -*- lexical-binding: t; -*-
;;; Commentary:
;;
;; Functions used by pspmacs configuration
;;
;;; Code:

(defun pspmacs/home-splash-before ()
  "run functions before switching to splash buffer."
    (dashboard-refresh-buffer)
    (run-hooks 'pspmacs/home-splash-before-hook))

(defun pspmacs/home-splash ()
  "Visit home screen."
  (interactive)
  (progn
    (pspmacs/home-splash-before)
    (setq-default default-directory "~/")
    (run-hooks 'pspmacs/home-splash-hook)))

(defun pspmacs/mode-prettify (sub-modes)
  "Apply pretiffy mode alist according to active-mode.

Load prettify-symbols from Each of SUB-MODES."
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

(defun pspmacs/prettify-R ()
  "Prettify ess-R"
  pspmacs/mode-prettify '("code" "R"))

(defun pspmacs/rfaces ()
  "R callable hook function"
  (font-lock-add-keywords
   'R-mode
   '(("\\W\\(\\(\\s_\\|\\sw\\|\\.\\)+\\)::"
      1 pspmacs/r-namespace-face t)))
  (font-lock-add-keywords
   'R-mode
   '(("\\w::\\(\\(\\s_\\|\\sw\\|\\.\\)+\\)"
      1 pspmacs/r-name-obj-face t)))
  (font-lock-add-keywords
   'R-mode
   '(("\\(\\(\\s_\\|\\sw\\|\\.\\)+\\)\\$\\w"
      1 pspmacs/r-list t)))
  (font-lock-add-keywords
   'R-mode
   '(("\\w\\$\\(\\(\\s_\\|\\sw\\|\\.\\)+\\)"
      1 pspmacs/r-list-obj t))))

(defun pspmacs/pyfaces ()
  "Python keyword faces"
  (font-lock-add-keywords
   'python-mode '(("\\W\\(\\*\\{1,2\\}\\(\\s_\\|\\sw\\|\\.\\)+\\)"
                   1 pspmacs/pyargs-face t)))
  (font-lock-add-keywords
   'python-mode '(("\\W\\(_\\{1,2\\}\\(\\s_\\|\\sw\\|\\.\\)+\\)"
                   1 pspmacs/pydunder-face t)))
  (font-lock-add-keywords
   'python-mode '(("``\\(.*?\\)``"
                   1 pspmacs/rst-literal-face t))))

(defun pspmacs/prettify-emacs-lisp ()
  "Prettify Emacs-Lisp"
    (pspmacs/mode-prettify '("code" "emacs-lisp")))

(defun pspmacs/prettify-note ()
  (pspmacs/mode-prettify '("lisp" "org")))

(defun pspmacs/load-modules (&optional modules-order)
  "Load modules in order.

Load modules as defined in MODULES-ORDER.
Defaults to the variable pspmacs/modules-order"
  (let* ((modules-order (or modules-order pspmacs/modules-order)))
    (seq-doseq (autofile modules-order nil)
      (catch 'load-success
        (dolist (work-tree pspmacs/worktrees nil)
          (let* ((lit-module
                  (expand-file-name
                   (format "modules/pspmacs-%s.org" autofile) work-tree))
                 (found (when (file-readable-p lit-module)
                          (pspmacs/load-suitable lit-module)
                          lit-module)))
            (when found (throw 'load-success lit-module))))))))

(defun pspmacs/byte-compile-worktrees (&optional worktree)
  "Byte-compile directory recursively.

Target: WORKTREE.
Default worktree is global (`user-emacs-directory)
This may be disabled by setting `pspmacs/byte-worktree' to nil"
  (when pspmacs/byte-worktree
    (let ((worktree (or worktree user-emacs-directory)))
      (byte-recompile-directory worktree 0))))

(defun pspmacs/inferior-interpreter (executable)
  "Open an inferior interpreter in split window.

Open EXECUTABLE interpreter in an inferior windows."
  (interactive)
  (let ((interpreter-window (split-window-below)))
    (select-window interpreter-window)
    (call-interactively executable)))

(defun pspmacs/destroy-buffer-and-window (&optional target-buffer)
  "Destroy window and buffer after some process is done.

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

Add each element from ELEMENTS to LIST-VAR.
Return value is the new value of LIST-VAR."
  (unless (listp elements)
    (error "ELEMENTS must be list"))
  (dolist (elem elements)
    (add-to-list list-var elem))
  (symbol-value list-var))

(defun pspmacs/maj-cond-call (callback maj-modes)
  "Run CALLBACK unless major mode is any of MAJ-MODES.

If MAJ-MODES is a list, `major-mode' shouldn't be in MAJ-MODES."
  (let ((maj-modes-list
         (if (listp maj-modes) maj-modes `(,maj-modes))))
    (unless (member major-mode maj-modes-list)
      (call-interactively callback))))

(defun pspmacs/modus-themes-custom-faces ()
  "Customize modus theme faces."
  (modus-themes-with-colors
    (progn
      (custom-set-faces
       ;; Add "padding" to the mode lines
       `(hl-line ((,c :slant italic)))
       `(org-document-title ((,c :foreground "#ffff9f")))
       `(font-function-name-face ((,c :foreground "#9f5f9f" :weight bold)))
       `(font-lock-comment-face ((,c :foreground "#3f4f5f" :background "#0f0f0f")))
       `(line-number ((,c :foreground "#4f5f7f" :background "#000000")))
       `(font-lock-type-face ((,c :foreground "#ff3f5f" :weight bold)))
       `(font-lock-rpack-face ((,c :foreground "#9f7fff")))
       `(font-lock-relem-face ((,c :foreground "#bf8faf")))
       `(font-lock-rsuper-face ((,c :foreground "#8fafbf")))
;;        `(mode-line ((,c :underline ,border-mode-line-active
;;                         :overline ,border-mode-line-active
;;                         :box (:line-width 10 :color ,bg-mode-line-active))))
;;        `(mode-line-inactive
;;          ((,c :underline ,border-mode-line-inactive
;;               :overline ,border-mode-line-inactive
;;               :box (:line-width 10 :color ,bg-mode-line-inactive))))
       ))))

(defun pspmacs/projectile-find-file-all ()
  (interactive)
  (let ((projectile-git-command "git ls-files -zco"))
(projectile-find-file)))

(defun pspmacs/orderless-dispatch-flex-first (_pattern index _total)
  (and (eq index 0) 'orderless-flex))

(defun pspmacs/lsp-mode-setup-completion ()
  (setf (alist-get 'styles (alist-get 'lsp-capf completion-category-defaults))
        '(orderless)))

(defun pspmacs/lsp-ui-disable-modes ()
  "Disable certian modes from lsp-ui"
  (display-line-numbers-mode -1)
  (whitespace-mode -1))

(defun pspmacs/pytest-use-venv (orig-fun &rest args)
  (if-let ((python-pytest-executable (executable-find "pytest")))
      (apply orig-fun args)
    (apply orig-fun args)))

(defun pspmacs/yank-file-name ()
  "Yank file-name to clipboard

Also, display file name in echo area"
  (interactive)
  (kill-new buffer-file-name)
  (message (format "Copied: %s"buffer-file-name)))

(defun wl-copy (text)
  "Copy to wayland clipboard.

Copy TEXT to wayland wl-copy"
  (setq wl-copy-process (make-process :name "wl-copy"
                  :buffer nil
                  :command '("wl-copy" "-f" "-n")
                  :connection-type 'pipe))
  (process-send-string wl-copy-process text)
  (process-send-eof wl-copy-process))

(defun wl-paste ()
  "Paste from wayland clipboard."
  (if (and wl-copy-process (process-live-p wl-copy-process))
  nil ; should return nil if we're the current paste owner
    (shell-command-to-string "wl-paste -n | tr -d \r")))

(defun pspmacs/project-to-publish-alist
    (org-root html-root org-templates)
  "Set root locations for source ORG-ROOT and target HTML-ROOT

to publish orgmode files to html."
  (interactive
   (let (org-root html-root org-templates)
     (setq org-root (read-directory-name
                     "ORG Directory:\t"
                     nil default-directory
                     ".*" nil))
     (setq html-root (read-directory-name
                      "HTML Directory:\t"
                      (expand-file-name "../html" org-root) nil
                      ".*" nil))
     (setq org-templates (read-directory-name
                          "Templates Directory:\t"
                          (expand-file-name "templates"
                                            pspmacs/org-template-path)
                          nil ".*" nil))
     (list org-root html-root org-templates)))

  (catch 'pspmacs/mk-tag
    (unless (file-directory-p html-root)
      (if (yes-or-no-p (format "%s doesn't exist. Create? " html-root))
          (make-directory html-root t)
        (throw 'pspmacs/mk-tag nil)))
    (setq org-publish-project-alist
          (list
           (list "org-notes"
                 :base-directory org-root
                 :base-extension "org"
                 :publishing-directory html-root
                 :recursive t
                 :publishing-function 'org-html-publish-to-html
                 :headline-levels 4
                 :auto-preamble t)
           (list "org-static"
                 :base-directory org-root
                 :base-extension
                 "css\\|js\\|png\\|jpg\\|gif\\|pdf\\|mp3\\|ogg\\|swf"
                 :publishing-directory html-root
                 :recursive t
                 :publishing-function 'org-publish-attachment)
           (list "org-templates"
                 :base-directory org-templates
                 :base-extension
                 "css\\|js\\|png\\|jpg\\|gif\\|pdf\\|mp3\\|ogg\\|swf"
                 :publishing-directory html-root
                 :recursive t
                 :publishing-function 'org-publish-attachment)
           (list "org" :components
                 '("org-notes" "org-static" "org-templates"))))))

;;; func.el ends there

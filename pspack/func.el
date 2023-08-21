﻿;;; pspack/func.el --- common pspmacs functions -*- lexical-binding: t; -*-
;;; Commentary:
;;
;; Functions used by pspmacs configuration
;;
;;; Code:

(defun pspmacs--call-shell (program args &optional on-fail)
    "Call shell command from function.

Exceute PROGRAM in shell with ARGS.
On failure execute ON-FAIL if given.
Return STDOUT as a string
Copied and maybe modufied form module pspmacs-ci-cd."
    (message "Executing: %s args: %s" program args)
    (with-temp-buffer
      (let ((exit-code (apply 'call-process
                              `(,program nil ,(current-buffer) nil ,@args))))
        (unless (eq 0 exit-code)
          (when on-fail
            (switch-to-buffer (current-buffer))
            (eval on-fail)))
        (replace-regexp-in-string "\n$" "" (buffer-string)))))

(defun pspmacs/git-rebase ()
  "Synchronize by rebasing locally cloned worktree/git on remote git."
  (interactive)
  (let ((default-directory user-emacs-directory)
        (_ (pspmacs--call-shell "git" '("fetch" "origin")))
        (behind-by
         (string-to-number
          (pspmacs--call-shell
           "git" '("rev-list" "--count" "--right-only"
                   "HEAD...@{upstream}")))))
    (pspmacs--call-shell "git" '("rebase"))
    (if (> behind-by 0)
        (progn
          (when (string= "Restart" (completing-read
                                    "Restart for settings to take effect."
                                    '("Restart" "I'll do it myself")))
            (restart-emacs))
          (message "Remember to restart!"))
      (message "👍"))))

(defun pspmacs/fill-cap-color (perc &optional bright invert)
  "Color based on filled capacity percentage PERC.

=0= is empty (bad = red), =100= is filled (good = green)
Percent of brightness is provided through BRIGHT, else assumed as 100%
If INVERT is non-nil, goodness/badness is reversed.
PERC > 101 is interpreted as *overfilled* (returns BRIGHT magenta)"
  (if (> perc 100)
      (let ((bright (if bright (/ bright 100.0) 1)))
        (color-rgb-to-hex bright 0 bright 2))
    (let* ((frac (/ (if invert (- 100 perc) perc) 100.0))
           (bright (if bright (/ bright 100.0) 1))
           (red (* bright (* 2 (- 0.5 (max 0 (- frac 0.5))))))
           (green (* bright (* 2 (- 0.5 (max 0 (- 0.5 frac))))))
           (blue (* bright (* 10 (max 0 (- frac 0.9))))))
      (color-rgb-to-hex red green blue 2))))

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
  (pspmacs/mode-prettify '("code" "R")))

(defun pspmacs/rfaces ()
  "R callable hook function"
  (font-lock-add-keywords 'R-mode pspmacs/r-keywords))

(defun pspmacs/prettify-python ()
  "Prettify python"
  (pspmacs/mode-prettify '("code" "python")))

(defun pspmacs/pyfaces ()
  "Python keyword faces"
  (font-lock-add-keywords nil pspmacs/py-keywords))

(defun pspmacs/prettify-emacs-lisp ()
  "Prettify Emacs-Lisp"
  ;; (font-lock-add-keywords nil pspmacs/elisp-keywords)
    (pspmacs/mode-prettify '("code" "emacs-lisp")))

(defun pspmacs/prettify-note ()
  (pspmacs/mode-prettify '("lisp" "org")))

(defun pspmacs/prettify-rust ()
  (pspmacs/mode-prettify '("code" "rust")))

(defun pspmacs/set-font-faces ()
  (set-face-attribute 'default nil
                      :font "Fira Code"
                      :height pspmacs/font-height)

  ;; Set the fixed pitch face
  (set-face-attribute 'fixed-pitch nil
                      :font "Fira Code"
                      :height pspmacs/font-height)

  ;; Set italic font face if available
  (ignore-errors
    (set-face-attribute 'italic nil
                        :font "VictorMono"
                        :slant 'italic
                        :height pspmacs/font-height)

    ;; Set the variable pitch face
    (set-face-attribute 'variable-pitch nil
                        :font "Cantarell"
                        :height pspmacs/font-height
                        :weight 'regular)))

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
  (unless (and (boundp 'no-native-compile) no-native-compile)
    (when pspmacs/byte-worktree
      (let ((worktree (or worktree user-emacs-directory)))
        (byte-recompile-directory worktree 0)))))

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

(defun pspmacs/switch-to-minibuffer ()
  "Switch to minibuffer window."
  (interactive)
  (if (active-minibuffer-window)
      (select-window (active-minibuffer-window))
    (message "Minibuffer is not active")))

(defun pspmacs/kill-other-buffers ()
  "Kill all other buffers."
  (interactive)
  (when (y-or-n-p "Delete all other buffers?")
    (mapc 'kill-buffer
          (seq-reduce
           (lambda (x y) (delq y x))
           `(,(current-buffer) ,(get-buffer messages-buffer-name))
           (buffer-list)))
    (message "Deleted all other buffers.")))

(defun pspmacs/extend-list (list-var elements)
  "Iterative form of ‘add-to-list’.

Add each element from ELEMENTS to LIST-VAR.
Return value is the new value of LIST-VAR."
  (unless (listp elements)
    (user-error "ELEMENTS must be list"))
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
       `(font-lock-comment-face ((,c :foreground "#bfdfff"
                                     :background "#003050"
                                     :slant italic)))
       `(font-lock-doc-face ((,c :foreground "#ffdfbf"
                                 :background "#503000"
                                 :slant italic)))
       `(mode-line-buffer-id ((,c :foreground "#009f9f")))
       `(line-number ((,c :foreground "#4f5f7f" :background "#000000")))
       `(font-lock-type-face ((,c :foreground "#ff3f5f" :weight bold)))))))

(defun pspmacs/projectile-find-file-all ()
  (interactive)
  (let ((projectile-git-command "git ls-files -zco"))
(projectile-find-file)))

(defun pspmacs/orderless-dispatch-flex-first (_pattern index _total)
  (and (eq index 0) 'orderless-flex))

(defun pspmacs/eglot-capf ()
  (setq-local completion-at-point-functions
              (list (cape-super-capf
                     #'eglot-completion-at-point
                     #'tempel-expand
                     #'cape-file))))

(defun pspmacs/ignore-elisp-keywords (cand)
  (or (not (keywordp cand))
      (eq (char-after (car completion-in-region--data)) ?:)))

(defun pspmacs/setup-elisp ()
  (setq-local completion-at-point-functions
              `(,(cape-super-capf
                  (cape-capf-predicate
                   #'elisp-completion-at-point
                   #'pspmacs/ignore-elisp-keywords)
                  #'cape-dabbrev)
                cape-file)
              cape-dabbrev-min-length 5))

(defun pspmacs/pytest-use-venv (orig-fun &rest args)
  (if-let ((python-pytest-executable (executable-find "pytest")))
      (apply orig-fun args)
    (apply orig-fun args)))

(defun pspmacs/prefer-interpreter-ipython ()
  "Use ipython as the python interpreter if available.

This requires us to reset various regular expressions."
  (interactive)
  (when (executable-find "ipython")
    (setq python-shell-interpreter (executable-find "ipython")
          python-shell-interpreter-args "-i --simple-prompt --no-color-info"
          python-shell-prompt-regexp "In \\[[0-9]+\\]: "
          python-shell-prompt-block-regexp "\\.\\.\\.\\.: "
          python-shell-prompt-output-regexp "Out\\[[0-9]+\\]: "
          python-shell-completion-setup-code
          "from IPython.core.completerlib import module_completion"
          python-shell-completion-string-code
          "';'.join(get_ipython().Completer.all_completions('''%s'''))\n")))

(defun pspmacs/yank-file-name ()
  "Yank file-name to clipboard

Also, display file name in echo area"
  (interactive)
  (kill-new buffer-file-name)
  (message (format "Copied: %s" buffer-file-name)))

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

(defun pspmacs/drop-bom ()
  "Drop Byte Order Mark (BOM) that may get tangled at the beginning of buffer

Suggestion: add to `org-babel-post-tangle-hook'"
  (interactive)
  (let ((bom '(?\ufeff ?\ufffe ?\uffff))
        (current-point (point)))
    (goto-char (point-min))
    (when (member (char-after 1) bom)
      (delete-char 1)
      (message "BOM deleted"))
    (goto-char current-point)))

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

(defun pspmacs/org-paste-as-link ()
  "Paste contents of clipboard as link."
  (interactive)
  (let* ((link-loc (current-kill 0))
         (desc (read-string "Description:\t" link-loc)))
    (org-insert-link nil link-loc desc)))

(defun pspmacs/org-copy-link-at-point ()
  "Copy link if thing at point as link"
  (interactive)
  (let* ((context (org-element-context))
         (type (org-element-type context))
         )
    (when (eq type 'link)
      (kill-new (format "%s:%s"
                        (org-element-property :type context)
                        (org-element-property :path context))))))

(defun pspmacs/mode-scratch (&optional buffer-mode)
  "Create a scratch buffer with arbitrary major mode in BUFFER-MODE"
  (interactive)
  (let* ((buffer-mode (or buffer-mode 'lisp-interaction-mode))
         (buffer-string-prefix (string-trim-right
                                (if (symbolp buffer-mode)
                                    (symbol-name buffer-mode)
                                  buffer-mode)
                                "-mode"))
         (scratch-name (format "*%s scratch*" buffer-string-prefix))
         (scratch-notice
          (string-replace "Lisp evaluation"
                          (format "%s mode" buffer-string-prefix)
                          (string-replace ";; "
                                          nil initial-scratch-message))))
    (switch-to-buffer scratch-name)
    (with-current-buffer scratch-name
      (funcall-interactively buffer-mode)
      (when (= (buffer-size) 0)
        (insert (substitute-command-keys scratch-notice))
        (goto-char (point-min))
        (comment-line 2)
        (goto-char (point-max))))))

(defun pspmacs--org-pop-cookie (heading-cookie-re)
  "PRIVATE: used by `pspmacs/org-put-checkboxes'.

HEADING-COOKIE-RE: regular expression that recognises cookies"
  (save-excursion
    (goto-char (line-end-position))
    (while (re-search-backward heading-cookie-re (line-beginning-position) t)
      (replace-match "" nil nil)))
  (if (string= (org-get-todo-state) "TODO")
      (org-todo "")))

(defun pspmacs--org-push-cookie ()
  "PRIVATE: used by `pspmacs/org-put-checkboxes'."
  (end-of-line)
  (insert " [/]")
  (unless (org-get-todo-state)
    (org-todo "TODO")))

(defun pspmacs/org-map-plain-list (func)
  "Walk down the current heading to locate plain lists and map.

Allpy FUNC to all lines which qualify to be list items `org-at-item-p'"
  (save-excursion
    (forward-line 1)
    (while (and (not (eobp))
                (not (org-at-heading-p)))
      (when (org-at-item-p)
        (funcall func))
      (forward-line 1))))

(defun pspmacs/org-put-checkboxes (&optional negate called-recursively)
  "Mark current line with incomplete tags.

If current line is a heading, add a cookie '[/]' at the end.
If current is a list, add a checkbox '[ ]' at the beginning.
Pass otherwise or if already present.

If NEGATE is t, perform the opposite action, removing checkboxes and cookes
If CALLED-RECURSIVELY, don't update cookie statistics, that should be done
only at the end of recursion by the caller function.
"
  (interactive)
  (save-excursion
    (let
        ((line-text (buffer-substring-no-properties
                     (line-beginning-position)
                     (line-end-position)))
         (heading-cookie-re ".+\\(\\[[0-9]*/[0-9]*\\]\\)$"))
      (cond ((org-at-heading-p)
             ;; Handle Headings
             (if (string-match-p heading-cookie-re line-text)
                 (if negate
                     (pspmacs--org-pop-cookie heading-cookie-re))
               (pspmacs--org-push-cookie))
             (unless called-recursively
               (org-update-statistics-cookies t)))
            ((org-at-item-p)
             ;; Handle Lists
             (when (or (null (or (org-at-item-checkbox-p)
                                 negate))
                       (and (org-at-item-checkbox-p)
                            negate))
               (org-toggle-checkbox '(4))))))))

(defun pspmacs/org-put-checkboxes-recursively (&optional negate)
  "Mark current line with incomplete tags, iterating over org-subtree.

  Apply `pspmacs/org-put-checkboxes' recursively down the subtree;
  passing the optional argument NEGATE.
  "
  (interactive)
  (save-excursion
    (org-map-tree
     (lambda ()
       (pspmacs/org-put-checkboxes negate t)
       (org-map-entries
        (pspmacs/org-map-plain-list
         (lambda ()
           (pspmacs/org-put-checkboxes negate t)))
        nil
        'tree)))
    (org-update-statistics-cookies nil)))

(defun pspmacs/after-code-load (&rest _)
  "run after the program code file is loaded"
  (run-hooks 'pspmacs/after-code-load-hook))

(defun pspmacs/project-init (command)
  "Run after the program code file is loaded"
  (interactive
   `(,(read-string "pspmacs/project-init-command: "
                   pspmacs/project-init-command)))
  (run-hooks 'pspmacs/project-init-hook)
  (unless (string= command "")
    (message "Starting command %s" command)
    (let* ((command-parts (split-string command))
           (cmd (car command-parts))
           (args (cdr command-parts))
           (process-args `("project-init" "*project-init*" ,cmd ,@args)))
      (apply 'start-process process-args)
      (switch-to-buffer-other-window "*project-init*"))))

(defun pspmacs/serve-or-run (command)
  "Run after the program code file is loaded"
  (interactive
   `(,(read-string "serve-or-run-command: "
                   pspmacs/serve-or-run-command)))
  (run-hooks 'pspmacs/serve-or-run-hook)
  (unless (string= command "")
    (message "Starting command %s" command)
    (let* ((command-parts (split-string command))
           (cmd (car command-parts))
           (args (cdr command-parts))
           (process-args `("serve-or-run" "*serve-or-run*" ,cmd ,@args)))
      (apply 'start-process process-args)
      (switch-to-buffer-other-window "*serve-or-run*"))))

(defun pspmacs/at-org-header-p (&rest _)
  "Returns t if point is at potential org header

i.e. if at ^\\\\**$

All arguments are ignored"
  (string-match-p
   "^\\**$" (buffer-substring (line-beginning-position) (point))))

(defun pspmacs/at-org-in-buffer-settings-p (&rest _)
  "Returns t only if in-buffer settings tag '#+' is opened

t if point is at '^\\\\W*#'
all arguments are ignored"
  (string-match-p
   "^\\W*#" (buffer-substring (line-beginning-position) (point))))

(defmacro pspmacs/toggle (&optional var)
  "If VAR is non-nil, set it to nil else, t
VAR must be quoted"
  (let ((var (or var `(intern ,(symbol-name (read-variable "Variable: "))))))
    `(customize-set-variable ,var (not ,(eval var)))))

(defun pspmacs/toggle-var ()
  "A thin wrapper around macro `pspmacs/toggle'"
  (interactive)
  (eval '(pspmacs/toggle)))

(defun pspmacs/org-toggle-emphasis-display ()
  "Toggle org emphasis markers such as **, //, ~~, ==, ++"
  (interactive)
  (pspmacs/toggle 'org-hide-emphasis-markers))

;;; func.el ends there

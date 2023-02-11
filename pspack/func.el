;;; pspack/func.el --- common pspmacs functions -*- lexical-binding: t; -*-
;;; Commentary:
;;
;; Functions used by pspmacs configuration
;;
;;; Code:

(defcustom pspmacs/crooked-dir
  (file-name-as-directory
   (expand-file-name "crooked" local-emacs-directory))
  "Location to store cloned repos as an option to `straight.el'"
  :type '(string :tag "location to store cloned git repos"))

(defcustom pspmacs/git-fetcher-plist
  '("github" "https://www.github.com"
            "gitlab" "https://www.gitlab.com"
            "sourcehut" "https://git.sr.ht"
            "codeberg" "https://codeberg.org")
  "plist of git fetcher and base url."
  :type '(repeat (cons (string :tag "fetcher")
                       (string :tag "url"))))

(defun pspmacs/git-clone-args (melpa-style-recipe)
  "Try to create a repo url from melpa-style recipe.

MELPA-STYLE-RECIPE is parsed to create a repo url.
Default :FETCHER is github.
:HOST is interpreted as :FETCHER following `straight.el'.
:HOST supersedes :FETCHER.
:FILES, :COMMIT, :OLD-NAMES are ignored for now.
I don't know how to handle them..."
  (let* ((clone-args nil)
         (repo-parts (cdr melpa-style-recipe))
         (url (plist-get repo-parts :url))
         (repo (plist-get repo-parts :repo))
         (host-kw (plist-get repo-parts :host))
         (fetcher-kw (plist-get repo-parts :fetcher))
         (fetcher
          (lax-plist-get pspmacs/git-fetcher-plist
                         (or (if (symbolp host-kw)
                                 (symbol-name host-kw)
                               host-kw)
                             (if (symbolp fetcher-kw)
                                 (symbol-name fetcher-kw)
                               fetcher-kw)
                             "github")))
         ;; (commit (plist-get repo-parts :commit))
         ;; (files (plist-get repo-parts :files))
         ;; (old-names (plist-get repo-parts :old-names))
         (branch (plist-get repo-parts :branch))
         (url (cond
               ((and url (or (file-readable-p url)
                             (url-https-file-readable-p url)))
                url)
               ((and repo (or (file-readable-p repo)
                                  (condition-case nil
                                      (url-https-file-readable-p repo)
                                    (error nil))))
                repo)
               (t (format "%s/%s" fetcher repo)))))
    (add-to-list 'clone-args url)
    (when branch
      (add-to-list 'clone-args branch)
      (add-to-list 'clone-args "-b"))
    clone-args))

(defun pspmacs/crooked-git-clone (melpa-style-recipe)
  "Try what `straight.el' does, but crookedly.

This is a work-around to use git-cloned builds since the user does not
want to use `straight.el'.

Interpret MELPA-STYLE-RECIPE and clone repo.
Native-compile, add to `load-path'.
NO-CLONE NO-BUILD CAUSE INTERACTIVE are ignored.
Modified string is printed"
  (let* ((clone-args (pspmacs/git-clone-args melpa-style-recipe))
         (target-name (symbol-name (car melpa-style-recipe)))
         (target-path (expand-file-name target-name pspmacs/crooked-dir)))
    (if (file-exists-p target-path)
        (call-process "git" nil nil nil
                      "-C" target-path "pull" "--recurse-submodules")
      (progn
        (unless (eq (apply 'call-process
                           (append '("git" nil nil nil "clone"
                                     "--recurse-submodules")
                                   clone-args
                                   `(,target-path)))
                    0)
        (user-error "Can't clone with %s to %s" clone-args target-path))))
    (add-to-list 'load-path target-path)
    (ignore-errors (unless no-native-compile
                     (byte-recompile-directory target-path 0)))
    target-path))

(defun pspmacs/install-git-clone (melpa-style-recipe
                                   &optional
                                  no-clone
                                  no-build
                                  cause interactive)
  "Install packages by git-cloning its source code.

If the variable `pspmacs/install-git-clones' is nil, do nothing.
If package-manager is `straight', Simply use it, passing optional arguments
NO-CLONE NO-BUILD CAUSE INTERACTIVE to `straight-use-package'.
Else, clone the MELPA-STYLE-RECIPE, build it."
  (if pspmacs/install-git-clones
      (if (string= pspmacs/package-manager "straight")
          (straight-use-package melpa-style-recipe
                                no-clone
                                no-build
                                cause
                                interactive)
        (pspmacs/crooked-git-clone melpa-style-recipe))
    (message "Not cloning %s because `pspmacs/install-clones' is nil" melpa-style-recipe)))

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
  (font-lock-add-keywords 'R-mode pspmacs/r-keywords))

(defun pspmacs/prettify-python ()
  "Prettify python"
  (pspmacs/mode-prettify '("code" "python")))

(defun pspmacs/pyfaces ()
  "Python keyword faces"
  (font-lock-add-keywords nil pspmacs/py-keywords))

(defun pspmacs/prettify-emacs-lisp ()
  "Prettify Emacs-Lisp"
    (pspmacs/mode-prettify '("code" "emacs-lisp")))

(defun pspmacs/prettify-note ()
  (pspmacs/mode-prettify '("lisp" "org")))

(defun pspmacs/set-font-faces ()
  (set-face-attribute 'default nil
                      :font "Fira Code"
                      :height pspmacs/font-height)

  ;; Set the fixed pitch face
  (set-face-attribute 'fixed-pitch nil
                      :font "Fira Code"
                      :height pspmacs/font-height)

  ;; Set the variable pitch face
  (set-face-attribute 'variable-pitch nil
                      :font "Cantarell"
                      :height pspmacs/font-height
                      :weight 'regular))

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
  (unless no-native-compile
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

(defun pspmacs/kill-other-buffers ()
  "Kill all other buffers."
  (interactive)
  (mapc 'kill-buffer (delq (current-buffer) (buffer-list))))

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
       `(font-lock-comment-face ((,c :foreground "#3f4f5f" :background "#0f0f0f")))
       `(line-number ((,c :foreground "#4f5f7f" :background "#000000")))
       `(font-lock-type-face ((,c :foreground "#ff3f5f" :weight bold)))))))

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

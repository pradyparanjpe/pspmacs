;;; pull-emacs.el --- Install Emacs from GNU repository -*- lexical-binding: t; -*-
;;
;;; Commentary:
;; Clone [OR pull changes in] GNU Emacs, build it locally from repo and install.
;; TODO: Optionally, use transient
;;
;;; Code:

(defgroup emacs-repo nil
  "Build-installation of Emacs from repository."
  :group 'pspack)

(defconst emacs-repo/REPO-URL
  "git://git.sv.gnu.org/emacs.git"
  "URL of Emacs remote repo")

(defvar-local emacs-repo/build-anew nil
  "Decision to build Emacs anew.

Reasons:
- More than `stale-at' commits behind remote.
- Cloned anew.
- Changed branch.")

(defcustom emacs-repo/branch nil
  "Branch to follow. [if nil, <current>]

If cloning for the first time and nil, \"master\""
  :group 'emacs-repo
  :type '(choice (string :tag "Non-master branch to follow") (const nil)))

(defcustom emacs-repo/prefix
  (expand-file-name ".local" (getenv "HOME"))
  "Installation prefix."
  :group 'emacs-repo
  :type '(string :tag "Path to installation (without trailing 'bin')"))

(defcustom emacs-repo/clone-dir
  (expand-file-name
   "emacs/src"
   (or (getenv "XDG_DATA_HOME")
       (format "%s/.local/share" (getenv "HOME"))))
  "Clone Emacs repository at"
  :group 'emacs-repo
  :type '(string :tag "A clone of Emacs is maintained here."))

(defcustom emacs-repo/jobs
  (num-processors)
  "Number of allowed parallel jobs"
  :group 'emacs-repo
  :type '(number :tag "Parallel make"))

(defcustom emacs-repo/stale-at
  10
  "Local repo is considered stale if it is so many commits behind remote."
  :group 'emacs-repo
  :type '(number :tag "Tip: for 'always', set to -1"))

(defcustom emacs-repo/skip-check nil
  "If t, skip the step \"make-check\"."
  :group 'emacs-repo
  :type 'boolean)

(defcustom emacs-repo/assume nil
  "What to do when a step fails.

 if \"yes\", Proceed.
    if \"no\", die.
    if nil, ask interactively."
  :group 'emacs-repo
  :type '(choice (string :options '("yes" "no")) (const nil)))

(defcustom emacs-repo/config-flags nil
"Flags passed for \"./configure\".
    suggestions: --with-pgtk --with-mailutils --with-cairo --with-modules
        --with-gnutls\=ifavailable"
  :group 'emacs-repo
  :type '(repeat (string :tag "configuration flag")
                 :options '(--with-pgtk --with-cairo --with-modules
                                        --with-gnutls --with-mailutils
                                        --with-x --with-x-toolkit
                                        --with-pop --with-wide-int)))

(defun emacs-repo/call-shell (program args &optional on-fail)
  "Call shell command from function.

Exceute PROGRAM in shell with ARGS.
On failure execute ON-FAIL if given.
Return STDOUT as a string"
  (message "Executing: %s args: %s" program args)
  (with-temp-buffer
    (let ((exit-code (apply 'call-process
                            `(,program nil ,(current-buffer) nil ,@args))))
      (unless (eq 0 exit-code)
        (when on-fail
          (switch-to-buffer (current-buffer))
          (eval on-fail)))
      (replace-regexp-in-string "\n$" "" (buffer-string)))))

(defun emacs-repo/check-dependencies (&rest deps)
  "Check if dependencies are met.

DEPS is a list of shell-executables essential to build."
  (dolist (req deps nil)
    (unless (executable-find (if (symbolp req)
                                 (symbol-name req)
                               req))
      (error (format "'%s' not found" req)))))

(defun emacs-repo/check-accessible ()
  "Check if installation is accessible, warn otherwise.

Check if PREFIX/bin is in $PATH."
  (interactive)
  (let ((system-path (string-split (getenv "PATH") ":" t)))
    (unless (member (expand-file-name "bin" emacs-repo/prefix) system-path)
      (message "WARNING: %s/bin is not in PATH." emacs-repo/prefix))))

(defun emacs-repo/get-confirmation ()
  "Confirm that despite some failure, do we still wish to continue.

If ASSUME is \"yes\" proceed without prompt.
If ASSUME is \"no\",throw error without prompt.
if ASSUME is nil, prompt what to do?"
  (interactive)
  (cond ((string= assume "no")
         (error "Dying on first failure, since flag was set."))
        ((not emacs-repo/assume) ;; nil
         (if (y-or-n-p "The last step failed. Do you want to continue?")
             (message "Continuing, this may be risky.")
           (progn (message "Dying...")
                  (keyboard-escape-quit))))
        (t (message "Continuing despite failure, since flag was set."))))

(defun emacs-repo/get-remote ()
  "Get (clone/pull) from remote repo."
  (if (f-directory-p emacs-repo/clone-dir)
      (let ((default-directory emacs-repo/clone-dir))
        (emacs-repo/git-rebase))
    (emacs-repo/git-clone)))

(defun emacs-repo/build-install ()
  "Build and Install Emacs."
  (let* ((default-directory emacs-repo/clone-dir)
         (nproc (number-to-string emacs-repo/jobs))
         (config-flags (mapcar (lambda (x)
                                 (if (symbolp x) (symbol-name x) x))
                               emacs-repo/config-flags)))
    (message "Building emacs from %s." default-directory)
    (message "Running autogen.sh")
    (emacs-repo/call-shell "sh" '("autogen.sh")
                           '(error "Failed './autogen.sh'"))
    (message "Configure with flags: %s to install at prefix %s."
             config-flags emacs-repo/prefix)
    (emacs-repo/call-shell "sh"
                           `("configure"
                             ,(format "--prefix=%s" emacs-repo/prefix)
                             ,@config-flags)
                           '(error "Failed './configure'"))
    (message "make with %s parallel jobs." nproc)
    (emacs-repo/call-shell "make" `("-j" ,nproc) '(error "Failed 'make'"))
    (unless emacs-repo/skip-check
      (message "make check")
      (emacs-repo/call-shell "make"
                             `("-j" ,nproc "check")
                             `(emacs-repo/get-confirmation)))
    (message "make install")
    (emacs-repo/call-shell "make" `("-j" ,nproc "install")
                           '(error "Failed 'make install'"))

    (message "Successfully installed Emacs @ %s/bin." emacs-repo/prefix)
    (message "make clean")
    (emacs-repo/call-shell "make" `("-j" ,nproc "clean"))))

(defun emacs-repo/current-branch ()
  "Currently checked-out branch."
  (let ((default-directory emacs-repo/clone-dir))
    (emacs-repo/call-shell "git" '("branch" "--show-current"))))

(defun emacs-repo/list-remote-branches ()
  "A list of remote branches"
  (let ((branches (emacs-repo/call-shell
                   "git"
                   '("--no-pager" "branch" "-r" "--list" "--format"
                     "%(refname:lstrip=3)" "origin/*"))))
    (split-string branches "\n" t)))

(defun emacs-repo/checkout-branch ()
  "Check out a different branch.

If BRANCH is not nil, checkout to that branch.
TIP: use suitable falue of `default-directory' with `let'"
  (when emacs-repo/branch
    (unless (string= emacs-repo/branch (emacs-repo/current-branch))
      (message "Checking out branch %s." emacs-repo/branch)
      (let ((avail-branches (emacs-repo/list-remote-branches)))
        (unless (member emacs-repo/branch avail-branches)
          (error "Requested branch is not in remote."))
        (emacs-repo/call-shell "git" `("checkout" ,emacs-repo/branch))
        (unless (emacs-repo/call-shell
                 "git"
                 `("rev-parse" "--abbrev-ref"
                   ,(format "%s@{u}" emacs-repo/branch)))
          (emacs-repo/call-shell "git"
                                 `("branch" "--set-upstream-to"
                                   ,(format "origin/%s" emacs-repo/branch)))))
      (setq-local emacs-repo/build-anew t))))

(defun emacs-repo/remote-changed-p ()
  "Local repo is behind remote by so many commits.

When called interactively, a human message is displayed.
When called from function, t is returned if this number is greater than
the kwyword `stale-at' in the list `emacs-repo/build-args-list'"
  (interactive)
  (emacs-repo/call-shell "git" '("fetch" "origin"))
  (let* ((default-directory emacs-repo/clone-dir)
         (behind-by
          (string-to-number
           (emacs-repo/call-shell
            "git"
            '("rev-list" "--count" "--right-only" "HEAD...@{upstream}")))))
    (if (called-interactively-p 'interactive)
        (message "Behind by %s commits." behind-by)
      (< emacs-repo/stale-at behind-by))))

(defun emacs-repo/git-rebase ()
  "Refresh local repository.

TIP: use suitable falue of `default-directory' with `let'"
  (emacs-repo/checkout-branch)
  (when (emacs-repo/remote-changed-p)
    (message "Found remote changes, rebasing on them...")
    (emacs-repo/call-shell "git" '("rebase"))
    (setq-local emacs-repo/build-anew t)))

(defun emacs-repo/git-clone ()
  "Clone Emacs anew.

REPO-URL URL of Emacs repo.
CLONE-DIR location of local clone.
BRANCH Branch to follow."
  (let ((branch (or emacs-repo/branch "master")))
    (message "Cloning Emacs '%s' branch to %s" branch emacs-repo/clone-dir)
    (emacs-repo/call-shell "git" `("clone" "--branch" ,branch
                                   ,emacs-repo/repo-url ,emacs-repo/clone-dir)
                           '(error "Failed cloning repo.")))
  (setq-local emacs-repo/build-anew t))

(defun emacs-repo/repo-install ()
  "Clone [OR pull changes in] GNU Emacs, build it locally and install.

Arguments are read from custom-group `emacs-repo'"
  (interactive)
  (emacs-repo/check-dependencies 'git 'autoconf 'makeinfo 'make)

  ;; Set variables
  (emacs-repo/get-remote)
  (when emacs-repo/build-anew
    (emacs-repo/build-install)
    (emacs-repo/check-accessible))
  (message "Emacs is in sync with current remote.")
  (setq-local emacs-repo/build-anew nil))

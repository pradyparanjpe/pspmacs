;;; pull-emacs.el --- Install Emacs from GNU repository -*- lexical-binding: t; -*-
;;
;;; Commentary:
;; Clone [OR pull changes in] GNU Emacs, build it locally from repo and install.
;;
;;; Code:

(defcustom emacs-repo/build-args-list
  `(:branch nil
            :prefix ,(expand-file-name ".local" (getenv "HOME"))
            :clone-dir
            ,(expand-file-name
              "emacs/src"
              (or (getenv "XDG_DATA_HOME")
                  (format "%s/.local/share" (getenv "HOME"))))
           :jobs ,(num-processors)
           :skip-check nil
           :assume nil
           :config-flags nil)
  "Arguments used by `emacs-repo/repo-install'"
  :type '(repeat (cons (string :tag "keyword") (string :tag "value")))
  :group 'pspack)

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
      (buffer-string))))

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
  (let ((prefix (plist-get emacs-repo/build-args-list :prefix)))
    (unless (string-match-p (getenv "PATH")
                            (regexp-quote prefix))
      (message "WARNING: %s/bin is not in PATH." prefix))))

(defun emacs-repo/get-confirmation ()
  "Confirm that despite some failure, do we still wish to continue.

If ASSUME is \"yes\" proceed without prompt.
If ASSUME is \"no\",throw error without prompt.
if ASSUME is nil, prompt what to do?"
  (interactive)
  (let ((assume (plist-get emacs-repo/build-args-list :assume)))
    (cond ((string= assume "no")
           (error "Dying on first failure, since flag was set."))
          ((not assume)
           (if (y-or-n-p "The last step failed. Do you want to continue?")
               (message "Continuing, this may be risky.")
             (progn (message "Dying...")
                    (keyboard-escape-quit))))
          (t (message "Continuing despite failure, since flag was set.")))))

(defun emacs-repo/get-remote ()
  "Get (clone/pull) from remote repo."
  (let ((clone-dir (plist-get emacs-repo/build-args-list :clone-dir)))
    (if (f-directory-p clone-dir)
        (let ((default-directory clone-dir))
          (emacs-repo/git-rebase))
      (emacs-repo/git-clone))))

(defun emacs-repo/build-install ()
  "Build and Install Emacs."
  (let* ((default-directory (plist-get emacs-repo/build-args-list :clone-dir))
         (prefix (plist-get emacs-repo/build-args-list :prefix))
         (nproc (number-to-string
                 (plist-get emacs-repo/build-args-list :jobs)))
         (config-flags-string-or-symbol
          (plist-get emacs-repo/build-args-list :config-flags))
         (config-flags (mapcar (lambda (x)
                                 (if (symbolp x) (symbol-name x) x))
                               config-flags-string-or-symbol)))
    (message "Building emacs from %s." default-directory)
    (message "Running autogen.sh")
    (emacs-repo/call-shell "sh" '("autogen.sh")
                           '(error "Failed './autogen.sh'"))
    (message "Configure with flags: %s to install at prefix %s."
             config-flags prefix)
    (emacs-repo/call-shell "sh"
                           `("configure"
                             ,(format "--prefix=%s" prefix)
                             ,@config-flags)
                           '(error "Failed './configure'"))
    (message "make with %s parallel jobs." nproc)
    (emacs-repo/call-shell "make" `("-j" ,nproc) '(error "Failed 'make'"))
    (unless (plist-get emacs-repo/build-args-list :skip-check)
      (message "make check")
      (emacs-repo/call-shell "make"
                             `("-j" ,nproc "check")
                             `(emacs-repo/get-confirmation)))
    (message "make install")
    (emacs-repo/call-shell "make" `("-j" ,nproc "install")
                           '(error "Failed 'make install'"))

    (message "Successfully installed Emacs @ %s." "${prefix}/bin")
    (message "make clean")
    (emacs-repo/call-shell "make" `("-j" ,nproc "clean"))))

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
  (let ((branch (plist-get emacs-repo/build-args-list :branch)))
    (when branch
      (message "Checking out branch %s." branch)
      (let ((avail-branches (emacs-repo/list-remote-branches)))
        (unless (member branch avail-branches)
          (error "Requested branch is not in remote."))
        (emacs-repo/call-shell "git" `("checkout" ,branch))
        (unless (emacs-repo/call-shell
                 "git"
                 `("rev-parse" "--abbrev-ref" ,(format "%s@{u}" branch)))
          (emacs-repo/call-shell "git" `("branch" "--set-upstream-to"
                                         ,(format "origin/%s" branch))))))))

(defun emacs-repo/remote-changed-p ()
  "Check if porcelain is clean."
  (emacs-repo/call-shell "git" '("fetch" "origin"))
  (not
   (eq 0
      (let
          ((stdout
            (emacs-repo/call-shell
             "git"
             '("rev-list" "--count" "--left-right" "HEAD...@{upstream}"))))
        (string-match "\\([[:digit:]]+\\)\\W+\\([[:digit:]]+\\)" stdout)
        (string-to-number (match-string 2 stdout))))))

(defun emacs-repo/git-rebase ()
  "Refresh local repository.

TIP: use suitable falue of `default-directory' with `let'"
  (emacs-repo/checkout-branch)
  (when (emacs-repo/remote-changed-p)
    (message "Found remote changes, rebasing on them...")
    (emacs-repo/call-shell "git" '("rebase"))
    t))

(defun emacs-repo/git-clone ()
  "Clone Emacs anew.

REPO-URL URL of Emacs repo.
CLONE-DIR location of local clone.
BRANCH Branch to follow."
  (let ((repo-url (plist-get emacs-repo/build-args-list :repo-url))
        (branch (or (plist-get emacs-repo/build-args-list :branch) master))
        (clone-dir (plist-get emacs-repo/build-args-list :clone-dir)))
    (message "Cloning Emacs '%s' branch to %s" branch clone-dir)
    (emacs-repo/call-shell "git" `("clone" "--branch" ,branch
                                   ,repo-url ,clone-dir)
                           '(error "Failed cloning repo.")))
  t)

(defun emacs-repo/repo-install ()
  "Clone [OR pull changes in] GNU Emacs, build it locally and install.

Arguments are read from `emacs-repo/build-args-list'with following keys . values:
NOTE: all values are necessary. use `plist-put' to change desired values,
DO NOT redefine this list.
:branch Branch to follow. [if nil, <current> or \"master\"]
:prefix Installation prefix. [if nil, '${HOME}/.local']
:clone-dir clone Emacs at [if nil, \'${XDG_DATA_HOME:-${HOME}/.local/share}/emacs/src\']
:jobs Number of allowed parallel jobs. [if nil, number of processors]
:skip-check if t, skip the stop \'make-check\'.
:assume if \"yes\", Proceed even if hurdles appear;
    if \"no\", die at the first hurdle;
    if nil, ask interactively.
:config-flags Flags passed for \'./configure\'
    suggestions: --with-pgtk --with-mailutils --with-cairo --with-modules
        --with-gnutls\=ifavailable
"
  (interactive)
  (emacs-repo/check-dependencies 'git 'autoconf 'makeinfo)

  ;; Set variables
  (when (emacs-repo/get-remote)
    (emacs-repo/build-install)
    (emacs-repo/check-accessible))
  (message "Emacs is in sync with current remote."))


;; pull-emacs.el ends here

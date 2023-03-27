;;; pdpmacs-emacs-ci-cd.el --- Install Emacs from GNU repository -*- lexical-binding: t; -*-
;;
;; Copyright (c) 2023 Pradyumna Swanand Paranjape <pradyparanjpe@rediffmail.com>
;;
;;; Commentary:
;; Clone [OR pull changes in] GNU Emacs, build it locally from repo and install.
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
  100
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

(defun emacs-repo/check-accessible ()
  "Check if installation is accessible, warn otherwise.

Check if PREFIX/bin is in $PATH."
  (interactive)
  (let ((system-path (string-split (getenv "PATH") ":" t)))
    (if (member (expand-file-name "bin" emacs-repo/prefix) system-path) t
      (message "WARNING: %s/bin is not in PATH." emacs-repo/prefix))))

(defun emacs-repo/get-confirmation ()
  "Confirm that despite some failure, do we still wish to continue.

If ASSUME is \"yes\" proceed without prompt.
If ASSUME is \"no\",throw error without prompt.
if ASSUME is nil, prompt what to do?"
  (interactive)
  (cond ((string= emacs-repo/assume "no")
         (error "Dying on first failure, since flag was set."))
        ((not emacs-repo/assume) ;; nil
         (if (y-or-n-p "The last step failed. Do you want to continue?")
             (message "Continuing, this may be risky.")
           (progn (message "Dying...")
                  (keyboard-escape-quit))))
        (t (message "Continuing despite failure, since flag was set."))))

(defun emacs-repo/remote-changed-p ()
  "Local repo is behind remote by so many commits.

When called interactively, a human message is displayed.
When called from function, t is returned if this number is greater than
the kwyword `stale-at' in the list `emacs-repo/build-args-list'"
  (interactive)
  (let* ((default-directory emacs-repo/clone-dir)
         (_ (emacs-repo/call-shell "git" '("fetch" "origin")))
         (behind-by
          (string-to-number
           (emacs-repo/call-shell
            "git" '("rev-list" "--count" "--right-only"
                    "HEAD...@{upstream}")))))
    (if (called-interactively-p 'interactive)
        (message "Behind by %s commits." behind-by)
      (< emacs-repo/stale-at behind-by))))

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

(use-package transient)
(require 'transient)

(defconst emacs-repo/config-flag-options
  '("all" "cairo" "dbus" "gconf" "gif" "gnutls" "gsettings" "imagemagick" "jpeg"
    "mailutils" "modules" "native-compilation" "pgtk" "png" "pop" "rsvg" "sound"
    "tiff" "toolkit-scroll-bars" "webp" "wide-int" "x" "x-toolkit" "xim"
    "xinput2" "xpm")
  "Known emacs configuration flags")

(defvar with-switches
  (let ((with-switches nil) (used-shorts nil))
    (dolist (flag emacs-repo/config-flag-options nil)
      (let ((shortflag nil)
            (sublen 0)
            (flaglet (string-replace "-" "" flag)))
        (while (or (not shortflag) (member shortflag used-shorts))
          (setq sublen (1+ sublen))
          (setq shortflag (substring flaglet 0 sublen)))
        (push shortflag used-shorts)
        (let ((flag-with (format "--with-%s" flag))
              (flag-val (format "+%s" flag)))
          (add-to-list
           'with-switches
           `(,(format "%s+" shortflag) ,flag-with ,flag-val
             :always-read t
             :init-value
             (lambda (obj)
               (oset obj value
                     (if (or (member ,flag-with
                                     emacs-repo/config-flags)
                             (member (intern ,flag-with)
                                     emacs-repo/config-flags))
                         ,flag-val))))
           t))))
    with-switches)
  "Create compilation flag switches \"with\" (ON)")

(defvar without-switches
  (let ((without-switches nil) (used-shorts nil))
    (dolist (flag emacs-repo/config-flag-options nil)
      (let ((shortflag nil)
            (sublen 0)
            (flaglet (string-replace "-" "" flag)))
        (while (or (not shortflag) (member shortflag used-shorts))
          (setq sublen (1+ sublen))
          (setq shortflag (substring flaglet 0 sublen)))
        (push shortflag used-shorts)
        (let ((flag-without (format "--without-%s" flag))
              (flag-val (format "-%s" flag)))
          (add-to-list
           'without-switches
           `(,(format "%s-" shortflag) ,flag-without ,flag-val
             :always-read t
             :init-value
             (lambda (obj)
               (oset obj value
                     (if (or (member ,flag-without
                                     emacs-repo/config-flags)
                             (member (intern ,flag-without)
                                     emacs-repo/config-flags))
                         ,flag-val))))
           t))))
    without-switches)
  "Create compilation flag switches \"without\" (OFF)")

(transient-define-suffix emacs-repo/save-vars ()
  "Report the PREFIX-ARG, prefix's scope, and infix values."
  (interactive)
  (let ((args (transient-args (oref transient-current-prefix command))))
    (dolist
        (e-r--var '("assume" "branch" "clone-dir" "prefix" "REPO-URL") nil)
      (customize-set-variable
       (intern (format "emacs-repo/%s" e-r--var))
       (transient-arg-value (format "%s=" e-r--var) args)))
    (dolist (e-r--var '("jobs" "stale-at") nil)
      (customize-set-variable
       (intern (format "emacs-repo/%s" e-r--var))
       (string-to-number
        (transient-arg-value (format "%s=" e-r--var) args))))
    (dolist (e-r--var '("skip-check") nil)
      (customize-set-variable
       (intern (format "emacs-repo/%s" e-r--var))
       (transient-arg-value e-r--var args)))))

(transient-define-suffix emacs-repo/put-config-flags ()
  "Set configuration flags"
  (interactive)
  (let ((args (transient-args (oref transient-current-prefix command))))
    (print args)
    (customize-set-variable 'emacs-repo/config-flags nil)
    (dolist (flag-var emacs-repo/config-flag-options nil)
      (cond ((transient-arg-value (format "+%s" flag-var) args)
             (add-to-list 'emacs-repo/config-flags
                          (format "--with-%s" flag-var)))
            ((transient-arg-value (format "-%s" flag-var) args)
             (add-to-list 'emacs-repo/config-flags
                          (format "--without-%s" flag-var)))))))

(transient-define-suffix emacs-repo/qualify-config-flags ()
  "Add qualifiers to configure flags"
  (interactive)
  (let ((args (transient-args (oref transient-current-prefix command))))
    (dolist (flag-var emacs-repo/config-flags nil)
      (let ((qual-value (transient-arg-value (format "%s=" flag-var) args)))
        (when qual-value
          (ert--remove-from-list 'emacs-repo/config-flags flag-var)
          (add-to-list 'emacs-repo/config-flags
                       (format "%s=%s" flag-var qual-value)))))))

(transient-define-prefix emacs-repo/set-config-flags ()
  "Set flags for './configuration'"
  :incompatible
  (mapcar (lambda (x)
            `(,(format "+%s" x) ,(format "-%s" x)))
          emacs-repo/config-flag-options)
  [["With flags"
    :setup-children
    (lambda (_)
      (transient-parse-suffixes
       transient--prefix
       (apply 'vector with-switches)))]
   ["Without flags"
    :setup-children
    (lambda (_)
      (transient-parse-suffixes
       transient--prefix
       (apply 'vector without-switches)))]
   ["Save" ("S" "save" emacs-repo/put-config-flags)]])

(transient-define-prefix emacs-repo/add-flag-qual ()
  "Add qualifier to config-args"
  [:description
   "Configure args values."
   [:description
    "flags"
    :setup-children
   (lambda (_)
     (transient-parse-suffixes
      transient--prefix
      (apply
       'vector
       (mapcar
        (lambda (x)
          `(,(car (last (split-string x "-"))) ,(substring x 2) ,(format "%s=" x)))
       emacs-repo/config-flags))))]
   [:description "Save" ("S" "save" emacs-repo/qualify-config-flags)]])

(transient-define-prefix emacs-repo/variables ()
  "Show current value of variable"
  ;; (interactive)
  [:description
   "Variables"
   ("k" "skip-check" "skip-check"
    :always-read t
    :init-value (lambda (obj)
                  (if emacs-repo/skip-check
                      (oset obj value "skip-check"))))
   ("a" "assume" "assume="
    :init-value (lambda (obj)
                  (oset obj value emacs-repo/assume)))
   ("b" "branch" "branch="
    :init-value (lambda (obj)
                  (if emacs-repo/branch
                      (oset obj value emacs-repo/branch))))
   ("d" "clone-dir" "clone-dir="
    :init-value (lambda (obj)
                  (oset obj value emacs-repo/clone-dir)))
   ("j" "jobs" "jobs="
    :init-value
    (lambda (obj)
      (oset obj value
            (if emacs-repo/jobs
                (number-to-string emacs-repo/jobs)))))
   ("p" "prefix" "prefix="
    :init-value (lambda (obj)
                  (oset obj value emacs-repo/prefix)))
   ("s" "stale-at" "stale-at="
    :init-value (lambda (obj)
                  (oset obj value
                        (if emacs-repo/stale-at
                            (number-to-string
                             emacs-repo/stale-at)))))
   ("u" "REPO-URL" "REPO-URL="
    :init-value (lambda (obj) (oset obj value emacs-repo/REPO-URL)))]
  [:description "Save" ("S" "save" emacs-repo/save-vars)])

(transient-define-prefix emacs-repo ()
  "Pull Emacs from repo if stale, build and install it.

Main Entry-point."
  [:description
    "Pull, Build and Install Emacs from Git. (CI-CD)"
    [:description "Check"
                  ("a" "PREFIX accessibility"
                   (lambda ()
                     (interactive)
                     (if (emacs-repo/check-accessible)
                         (message "👍"))))
                  ("c" "Remote changes" emacs-repo/remote-changed-p)]
    [:description
     "Act"
     ("i" "Install" emacs-repo/repo-install)
     ("v" "Variables" emacs-repo/variables :transient t)]
    [:description
   "Configuration Flags"
     ("s" "Select" emacs-repo/set-config-flags :transient t)
     ("q" "Qualify" emacs-repo/add-flag-qual :transient t)]])
;; pspmacs-emacs-ci-cd.el ends here
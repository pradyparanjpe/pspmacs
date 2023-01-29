;;; early/definitions.el --- Personal configuration file -*- lexical-binding: t; no-byte-compile: t; -*-

(setq gc-cons-threshold most-positive-fixnum
      gc-cons-percentage 0.6)

(setq package-enable-at-startup nil
      package-quickstart nil)

(customize-set-variable 'load-prefer-newer t)

(defvar pvt-emacs-directory
  (file-name-as-directory
   (cond
    ((featurep 'chemacs)
     (if (getenv "PVT_EMACS_HOME")
         (expand-file-name (getenv "PVT_EMACS_HOME"))))
    ((getenv "PVT_EMACS_HOME")
     (expand-file-name (getenv "PVT_EMACS_HOME")))))
  "Private version controlled

privately synchronized configuration directory")

(defvar local-emacs-directory
  (file-name-as-directory
   (cond
    ((featurep 'chemacs)
     (if
         (getenv "LOCAL_EMACS_HOME")
         (expand-file-name (getenv "LOCAL_EMACS_HOME"))))
    ((getenv "LOCAL_EMACS_HOME")
     (expand-file-name (getenv "LOCAL_EMACS_HOME")))
    (t (cond
        (pvt-emacs-directory
         (expand-file-name "local.d" pvt-emacs-directory))
        (t
         (expand-file-name "local.d" user-emacs-directory))))))
  "Local, machine-specific, un-synchronized configuration directory")

(defvar pspmacs/user-worktrees
  `(,pvt-emacs-directory ,local-emacs-directory)
  "user's worktrees to load")
(defvar pspmacs/worktrees
  `(,user-emacs-directory ,pvt-emacs-directory ,local-emacs-directory)
  "worktrees to load")

(defvar pspmacs/load-custom-file t
  "When non-nil, load `custom.el' after `<user-emacs-config>/late/config.el'")

(defvar pspmacs/packaging-directory
  (expand-file-name "pspackaging" user-emacs-directory)
  "Packaging system (straight) to use.")

(defun pspmacs/load-inherit (&optional fname)
  "Inherit all equivalent files.

Files may be placed in `pvt-emacs-directory' and/or `local-emacs-directory'.
If FNAME is supplied, *that* corresponding file name is attempted, else,
stem of `load-file-name' is attempted."
  (let ((name-branch
     (file-relative-name (or fname load-file-name) user-emacs-directory)))
    (dolist (config-dir pspmacs/user-worktrees nil)
      (let* ((modular-init (expand-file-name name-branch config-dir)))
        (if (file-exists-p modular-init)
        (load modular-init nil 'nomessage))))))

(pspmacs/load-inherit)

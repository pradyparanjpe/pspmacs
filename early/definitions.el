;;; early/definitions.el --- Prune overloads -*- lexical-binding: t; no-byte-compile: t; -*-

(defvar pvt-emacs-dir
  (condition-case err (file-name-as-directory
                       (when (getenv "PVT_EMACS_HOME")
                         (file-name-as-directory (getenv "PVT_EMACS_HOME"))))
    ((error)
     (unless (string= (format "%s" err) "(wrong-type-argument stringp nil)")
       (throw 'uncaught err))))
  "Private version controlled.

Privately synchronized configuration directory.")

(defvar local-emacs-dir
  (file-name-as-directory
   (or (getenv "LOCAL_EMACS_HOME")
       (expand-file-name "local.d" (or pvt-emacs-dir user-emacs-directory))))
  "Local, machine-specific, un-synchronized configuration directory.")

(defvar pspmacs/user-worktrees (delq 'nil (list pvt-emacs-dir local-emacs-dir))
  "User's worktrees to load.")

(defvar pspmacs/worktrees
  (delq 'nil (list user-emacs-directory pvt-emacs-dir local-emacs-dir))
  "Worktrees to load.")

(defcustom pspmacs/load-custom-file t
  "When non-nil, load `custom.el' after `<user-emacs-config>/late/config.el'.")

(defun pspmacs/load-suitable (fname &optional nag)
  "Load emacs init file FNAME.

If FNAME is found, load it.
If not found and only if NAG is non-nil, throw error.

This function is overwritten in late/definitions.el after the correct
org mode is loaded to include org-babel-load-file method"
  (if (file-readable-p fname) (load fname nil 'nomessage)
    (when nag (user-error (format "%s not found." fname)))))

(defun pspmacs/load-inherit (&optional fname)
  "Inherit all equivalent files.

Files may be placed in `pvt-emacs-dir' and/or `local-emacs-dir'.
Settings loaded from files located in `pvt-emacs-dir' are overwritten
by settings loaded from files located in `local-emacs-dir'.
If FNAME is supplied, *that* corresponding file name is attempted, else,
stem of `load-file-name' is attempted.

Init files are loaded using the function `pspmacs/load-suitable'."
  (dolist (config-dir pspmacs/user-worktrees)
    (let ((modular-init
           (expand-file-name
            (file-relative-name (or fname load-file-name) user-emacs-directory)
            config-dir)))
      (condition-case err (pspmacs/load-suitable modular-init)
        (t (message "Error while loading %s: %s" modular-init err))))))

(pspmacs/load-inherit)

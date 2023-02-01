;;; early/definitions.el --- Prune overloads -*- lexical-binding: t; no-byte-compile: t; -*-

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

(defun pspmacs/load-suitable (fname &optional nag)
   "Load emacs init file FNAME.

 If FNAME is found, load it and return.
 If not found and if NAG is `t', throw error. Default: return.

 This function is overwritten in late/definitions.el after the correct
 org mode is loaded to include org-babel-load-file method"
   (if (file-readable-p fname)
       (load fname nil 'nomessage)
     (if nag (user-error (format "%s not found." fname)))))

(defun pspmacs/load-inherit (&optional fname)
  "Inherit all equivalent files.

 Files may be placed in `pvt-emacs-directory' and/or `local-emacs-directory'.
 Settings loaded from files located in `pvt-emacs-directory' are overwritten
 by settings loaded from files located in `local-emacs-directory'.
 If FNAME is supplied, *that* corresponding file name is attempted, else,
 stem of `load-file-name' is attempted.

 Init files are loaded using the function `pspmacs/load-suitable'."
  (let ((name-branch
         (file-relative-name
          (or fname load-file-name) user-emacs-directory)))
    (dolist (config-dir pspmacs/user-worktrees nil)
      (let ((modular-init (expand-file-name
                           name-branch config-dir)))
        (condition-case err
            (pspmacs/load-suitable modular-init)
          (t (message
              "Error while loading %s: %s" modular-init err)))))))

(pspmacs/load-inherit)

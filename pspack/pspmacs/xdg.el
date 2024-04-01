;;; xdg.el --- Personal xdg-definitions file -*- lexical-binding: t; -*-
;;; Commentary:
;;
;; XDG file-system awareness for pspmacs
;;
;;; Code:
(defgroup xdg nil
  "XDG awareness for pspmacs."
  :group 'pspmacs)

(defun xdg/assign-base (xdg-var default-path)
  "Emacs's default xdg-specified locations.

If xdg's variable is defined by system, use it, else return default
If XDG-VAR is defined in the environment, use XDG-VAR/emacs,
else use DEFAULT-PATH/emacs"
  (directory-file-name
   (expand-file-name "emacs" (or (getenv xdg-var) default-path))))

(defcustom xdg/emacs-config-directory
  (eval user-emacs-directory)
  "Location of local machine-specific Emacs-configuration files.

alias of USER_EMACS_DIRECTORY"
  :group 'xdg
  :type '(string :tag "the ~/.emacs.d/"))

;; ${XDG_CACHE_HOME:-${HOME}/.cache}/emacs
(defcustom xdg/emacs-cache-directory
  (xdg/assign-base "XDG_CACHE_HOME" "~/.cache/")
  "Location of runtime cache files for Emacs.

${XDG_CONFIG_HOME:-${HOME}/.cache}/emacs"
  :group 'xdg
  :type '(string :tag "Path to cache"))

;; ${XDG_DATA_HOME:-${HOME}/.local/share}/emacs
(defcustom xdg/emacs-data-directory
  (xdg/assign-base "XDG_DATA_HOME" "~/.local/share")
  "Location of persistent data files for Emacs.

${XDG_DATA_HOME:-${HOME}/.local/share}/emacs"
  :group 'xdg
  :type '(string :tag "Path to persistent data"))

;; ${XDG_STATE_HOME:-${HOME}/.local/state}/emacs
(defcustom xdg/emacs-state-directory
  (xdg/assign-base "XDG_STATE_HOME" "~/.local/state")
  "Location of volatile state files for Emacs.

${XDG_STATE_HOME:-${HOME}/.local/state}/emacs"
  :group 'xdg
  :type '(string :tag "Path for logging"))

(dolist (xdg-base '(xdg/emacs-data-directory
                    xdg/emacs-cache-directory
                    xdg/emacs-state-directory))
  (make-directory (eval xdg-base) t))

(defun xdg/make-path (var &optional base)
  "Generate xdg/emacs path.

Construct path for VAR relative to xdg/emacs-BASE-directory.  BASE can be
data [default], config, cache, state."
  (convert-standard-filename
   (expand-file-name
    var (eval (intern (format "xdg/emacs-%s-directory" (or base "data")))))))

(defun locate-user-emacs-file (new-name &optional old-name)
  "This function overwrites Emacs-native function.

Return an absolute per-user Emacs-specific file name.
If NEW-NAME exists in `xdg/emacs-cache-directory', return it.
Else if OLD-NAME is non-nil and ~/OLD-NAME exists, return ~/OLD-NAME.
Else return NEW-NAME in `xdg/emacs-cache-directory', creating the
directory if it does not exist."
  (convert-standard-filename
   (let* ((home (concat "~" (or init-file-user "")))
          (at-home (and old-name (expand-file-name old-name home)))
          (bestname (abbreviate-file-name (xdg/make-path new-name 'cache))))
     (if (and at-home (not (file-readable-p bestname))
              (file-readable-p at-home))
         at-home
       ;; Make sure `xdg/emacs-cache-directory' exists,
       ;; unless we're in batch mode or dumping Emacs.
       (or noninteractive
           dump-mode
           (let (errtype)
             (if (file-directory-p xdg/emacs-cache-directory)
                 (or (file-accessible-directory-p xdg/emacs-cache-directory)
                     (setq errtype "access"))
               (with-file-modes ?\700
                 (condition-case nil
                     (make-directory xdg/emacs-cache-directory t)
                   (error (setq errtype "create")))))
             (when (and errtype
                        user-emacs-directory-warning
                        (not (get 'xdg/emacs-cache-directory-warning
                                  'this-session)))
               ;; Warn only once per Emacs session.
               (put 'xdg/emacs-cache-directory-warning 'this-session t)
               (display-warning 'initialization
                                (format "\
Unable to %s `xdg/emacs-cache-directory' (%s).
Any data that would normally be written there may be lost!
If you never want to see this message again,
customize the variable `xdg/emacs-cache-directory-warning'."
                                        errtype xdg/emacs-cache-directory)))))
       bestname))))

(provide 'pspmacs/xdg)
;;; xdg.el ends here

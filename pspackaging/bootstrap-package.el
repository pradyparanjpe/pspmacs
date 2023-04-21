(defcustom pspmacs/package-manager
  (if (version< emacs-version "29")
      'straight
    'builtin)
  "Packaging system to use."
  :type '(string :tag "Emacs package manager to use")
  :options '(builtin straight))

(defcustom pspmacs/packaging-directory
  (expand-file-name "pspackaging" user-emacs-directory)
  "Packaging recipes directory."
  :type '(string :tag "Directory with modules to bootstrap package manager"))

(defcustom pspmacs/install-git-clones
  (if (string= pspmacs/package-manager "straight") t nil)
  "Install packages, which require git cloning."
  :type 'boolean)

(defun pspmacs/package-bootstrap (&optional manager)
  "Bootstrap package manager to install and configure emacs packages.

If MANAGER is a member of straight, cloned, straight.el,
`straight.el' package manager is bootstrapped.
Else, `pspmacs/package-manager', which defaults to the builtin
manager `package.el', is bootstrapped.

This is the first function called by `init.el'."
  (let* ((manager
          (if (member manager '(straight cloned straight.el))
              'straight
            pspmacs/package-manager))
         (manager-file (expand-file-name
                        (format "bootstrap-%s.el" manager)
                        pspmacs/packaging-directory)))
    (if (file-exists-p manager-file)
        (load manager-file nil 'nomessage)
      (error "Could not find bootstrap instructions for %s." manager))))

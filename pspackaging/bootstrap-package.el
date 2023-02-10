(defcustom pspmacs/package-manager
  'builtin
  "Packaging system to use"
  :type '(string :tag "Emacs package manager to use")
  :options '(builtin straight))

(defcustom pspmacs/packaging-directory
  (expand-file-name "pspackaging" user-emacs-directory)
  "Packaging recipes directory."
  :type '(string :tag "Directory with recipes to bootstrap package manager."))

(defcustom pspmacs/install-git-clones
  (if (string= pspmacs/package-manager "straight") t nil)
  "Install packages which requires git cloning."
  :type 'boolean)

(defun pspmacs/package-bootstrap (&optional manager)
  "Bootstrap package manager to install and configure emacs packages.

If MANAGER is a member of builtin, package, minimal,
Emacs' builtin manager `package.el' is bootstrapped.
Else, it `pspmacs/package-manager', which defaults to `straight.el',
is bootstrapped.

This is the first function called by `init.el'."
  (let* ((manager
          (if (member manager '(builtin package minimal))
              'builtin
            pspmacs/package-manager))
         (manager-file (expand-file-name
                        (format "bootstrap-%s.el" manager)
                        pspmacs/packaging-directory)))
    (if (file-exists-p manager-file)
        (load manager-file nil 'nomessage)
      (error "Could not find recipe for %s." manager))))

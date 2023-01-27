;;; late/init.el --- Late init file, loads modules -*- lexical-binding: t; -*-

(when (featurep 'chemacs)
  (customize-set-variable 'package-user-dir
                          (expand-file-name "elpa" local-emacs-directory)))

(load (expand-file-name "bootstrap-package.el" pspmacs/packaging-directory)
      nil 'nomessage)

(dolist (init-dir
         `(,user-emacs-directory
           ,pvt-emacs-directory
           ,local-emacs-directory) nil)
  (let ((modular-modules (expand-file-name "modules/" init-dir)))
    (when (file-directory-p modular-modules)
      (setq load-path
            (append (let ((load-path (list))
                          (default-directory modular-modules))
                      (add-to-list 'load-path modular-modules)
                      ;;(normal-top-level-add-to-load-path '("."))
                      (normal-top-level-add-subdirs-to-load-path)
                      load-path)
                    load-path)))))

(customize-set-variable 'custom-file
  (expand-file-name "custom.el" local-emacs-directory))

;; Make GC pauses faster by decreasing the threshold.
(setq gc-cons-threshold (* 2 1000 1000))

(load (expand-file-name "late/org-latest.el" user-emacs-directory))

(dolist (autofile
         '("interface-enhancement"
           "editing-enhancement"
           "emacs-lisp"
           "integration"
           "keys-cheat-sheet"
           "markup"
           "navigate"
           "note"
           "programming"
           "project-management"
           "python"
           "theme"
           "version-control")
         nil)
  (let ((lit-module
     (expand-file-name
      (format "modules/pspmacs-%s.org" autofile) user-emacs-directory)))
    (if (file-readable-p lit-module)
        (pspmacs/load-suitable lit-module))))

(pspmacs/load-inherit)

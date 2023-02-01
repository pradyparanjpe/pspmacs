;;; late/init.el --- Late init file, loads modules -*- lexical-binding: t; -*-

(customize-set-variable 'custom-file
  (expand-file-name "custom.el" local-emacs-directory))

(setq gc-cons-threshold (* 2 1000 1000))

(seq-doseq (autofile pspmacs/modules-order nil)
  (dolist (work-tree pspmacs/worktrees nil)
    (catch 'load-success
      (let* ((lit-module
              (expand-file-name
               (format "modules/pspmacs-%s.org" autofile) work-tree))
             (found (when (file-readable-p lit-module)
                      (pspmacs/load-suitable lit-module)
                      lit-module)))
        (when found (throw 'load-success lit-module))))))

(pspmacs/load-inherit)

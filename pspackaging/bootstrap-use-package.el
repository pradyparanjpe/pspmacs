(defun pspmacs/init-use-package ()
  "Initialize `use-package'"
  (if (string= pspmacs/package-manager "straight")
      (straight-use-package 'use-package)
    (unless (package-installed-p 'use-package)
      (package-install 'use-package)))
  (eval-and-compile
    (setq use-package-compute-statistics t)
    (setq use-package-always-ensure t)
    (setq use-package-always-defer t)
    (setq use-package-expand-minimally t)))

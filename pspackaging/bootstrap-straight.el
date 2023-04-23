;;; bootstrap-straight.el --- straight.el package manager -*- lexical-binding: t; -*-
;;; straight as package manager
(defun pspmacs/init-package-manager ()
  "Initialize `package.el' as the package manager"
  (setq straight-use-package-by-default t)
  (setq straight-recipes-gnu-elpa-use-mirror t)
  (defvar bootstrap-version)
  (let ((bootstrap-file
         (expand-file-name
          "straight/repos/straight.el/bootstrap.el"
          local-emacs-dir))
        (bootstrap-version 6))
    ;; moves the straight install directory to the local machine configuration
    (setq straight-base-dir local-emacs-dir
          straight-use-package-by-default t)
    (unless (file-exists-p bootstrap-file)
      (message "Bootstrap file not found. Downloading...")
      (with-current-buffer
          (url-retrieve-synchronously
           "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
           'silent 'inhibit-cookies)
        (goto-char (point-max))
        (eval-print-last-sexp)))
    (load bootstrap-file nil 'nomessage)))
;; bootstrap-straight.el ends here

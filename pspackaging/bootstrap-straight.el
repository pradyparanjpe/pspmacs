;;; bootstrap-straight.el --- straight.el packaging system -*- lexical-binding: t; -*-

;;; straight as packaging system
(setq straight-use-package-by-default t)
(setq straight-recipes-gnu-elpa-use-mirror t)
;(setq use-package-always-defer t)
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name
         "straight/repos/straight.el/bootstrap.el"
         local-emacs-directory))
      (bootstrap-version 6))
  ;; moves the straight install directory to the local machine configuration
  (setq straight-base-dir local-emacs-directory
        straight-use-package-by-default t)
  (unless (file-exists-p bootstrap-file)
    (message "Bootstrap file not found. Downloading...")
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))
(straight-use-package 'use-package)

(setq use-package-compute-statistics t)
(provide 'pspmacs-package/straight)
;; bootstrap-straight.el ends here

;;; early/init.el --- early init -*- lexical-binding: t; no-byte-compile: t; -*-

(if (file-exists-p pvt-emacs-directory)
  (add-to-list 'load-path (expand-file-name pvt-emacs-directory)))
(add-to-list 'load-path (expand-file-name local-emacs-directory))

(unless (file-exists-p local-emacs-directory)
  (mkdir local-emacs-directory t))

(when (featurep 'native-compile)
  ;; Silence compiler warnings as they can be pretty disruptive
  (setq native-comp-async-report-warnings-errors nil)

  ;; Make native compilation happen asynchronously
  (setq native-comp-deferred-compilation t)

  ;; Set the right directory to store the native compilation cache
  ;; NOTE the method for setting the eln-cache directory
  ;; depends on the emacs version
  (when (fboundp 'startup-redirect-eln-cache)
    (if (version< emacs-version "29")
        (add-to-list
          'native-comp-eln-load-path
          (convert-standard-filename
            (expand-file-name "var/eln-cache/" local-emacs-directory)))
        (startup-redirect-eln-cache
          (convert-standard-filename
            (expand-file-name "var/eln-cache/" local-emacs-directory)))))
  (add-to-list
    'native-comp-eln-load-path
    (expand-file-name "eln-cache/" local-emacs-directory)))

(load-theme 'deeper-blue t)

(defalias 'yes-or-no-p 'y-or-n-p)

(pspmacs/load-inherit)

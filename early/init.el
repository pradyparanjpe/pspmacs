;;; early/init.el --- early init -*- lexical-binding: t; no-byte-compile: t; -*-

(if (file-exists-p pvt-emacs-dir)
  (add-to-list 'load-path (expand-file-name pvt-emacs-dir)))
(add-to-list 'load-path (expand-file-name local-emacs-dir))

(load-theme 'deeper-blue t)

(unless (file-exists-p local-emacs-dir)
  (mkdir local-emacs-dir t))

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
            (expand-file-name "var/eln-cache/" local-emacs-dir)))
        (startup-redirect-eln-cache
          (convert-standard-filename
            (expand-file-name "var/eln-cache/" local-emacs-dir)))))
  (add-to-list
    'native-comp-eln-load-path
    (expand-file-name "eln-cache/" local-emacs-dir)))

(setq pspmacs/packaging-directory
      (expand-file-name "pspackaging" user-emacs-directory))
(setq pspmacs/package-manager 'builtin)
;; (setq pspmacs/install-git-clones nil)

(defalias 'yes-or-no-p 'y-or-n-p)

(pspmacs/load-inherit)

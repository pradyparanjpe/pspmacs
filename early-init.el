;;; early-init.el --- Early Emacs init -*- lexical-binding: t; no-byte-compile: t; -*-
;;; Early definitions init configs

(load (expand-file-name "early/prune.el" user-emacs-directory)
      nil 'nomessage)

(load (expand-file-name "early/definitions.el" user-emacs-directory)
      nil 'nomessage)

(load (expand-file-name "early/init.el" user-emacs-directory)
      nil 'nomessage)

(load (expand-file-name "early/config.el" user-emacs-directory)
      nil 'nomessage)

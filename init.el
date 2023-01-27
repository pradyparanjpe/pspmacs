;;; init.el --- Late Emacs init -*- lexical-binding: t; no-byte-compile: t; -*-
;;; late definitions init configs

(load (expand-file-name "late/definitions.el" user-emacs-directory)
      nil 'nomessage)

(load (expand-file-name "late/init.el" user-emacs-directory)
      nil 'nomessage)

(load (expand-file-name "late/config.el" user-emacs-directory)
      nil 'nomessage)

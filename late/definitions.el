;;; late/definitions.el --- define values -*- lexical-binding: t; -*-

(load (expand-file-name "late/version.el" user-emacs-directory) nil 'nomessage)

(defgroup pspmacs nil
  "PSPMacs Custom init configuration for Emacs"
  :version pspmacs/version)

(load (expand-file-name "late/xdg.el" user-emacs-directory) nil 'nomessage)

(load (expand-file-name "late/func.el" user-emacs-directory) nil 'nomessage)

(load (expand-file-name "late/vars.el" user-emacs-directory) nil 'nomessage)

(pspmacs/load-inherit)

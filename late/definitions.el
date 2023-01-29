;;; late/definitions.el --- identifies platform -*- lexical-binding: t; -*-

(defconst ON-LINUX   (eq system-type 'gnu/linux))
(defconst ON-MAC     (eq system-type 'darwin))
(defconst ON-BSD     (or ON-MAC (eq system-type 'berkeley-unix)))
(defconst ON-WINDOWS (memq system-type '(cygwin windows-nt ms-dos)))

(load (expand-file-name "late/xdg.el" user-emacs-directory) nil 'nomessage)

(pspmacs/load-inherit)

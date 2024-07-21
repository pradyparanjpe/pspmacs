;;; init.el --- Late Emacs init -*- lexical-binding: t; no-byte-compile: t; -*-
;;; late definitions init configs
(load (expand-file-name "late/package-management.el" user-emacs-directory)
      nil 'nomessage)

(load (expand-file-name "late/definitions.el" user-emacs-directory)
      nil 'nomessage)

(load (expand-file-name "late/init.el" user-emacs-directory)
      nil 'nomessage)

(load (expand-file-name "late/config.el" user-emacs-directory)
      nil 'nomessage)

(when pspmacs/load-custom-file
  (load custom-file t))
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-vc-selected-packages
   '((vc-use-package :vc-backend Git :url "https://github.com/slotThe/vc-use-package"))))

;;; late/config.el --- Personal configuration file -*- lexical-binding: t; -*-

(general-define-key :keymaps 'evil-motion-state-map "RET" nil)

(when pspmacs/load-custom-file
  (load custom-file t))

(setq default-directory "~/")

(pspmacs/load-inherit)

;;; late/config.el --- Personal configuration file -*- lexical-binding: t; -*-

(general-define-key :keymaps 'evil-motion-state-map "RET" nil)
(general-define-key :keymaps 'evil-insert-state-map "C-k" nil)

(when pspmacs/load-custom-file
  (load custom-file t))

(pspmacs/load-inherit)

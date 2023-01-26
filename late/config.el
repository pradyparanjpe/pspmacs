;;; late/config.el --- Personal configuration file -*- lexical-binding: t; -*-

(when pspmacs/load-custom-file
  (load custom-file t))

(setq default-directory "~/")

(pspmacs/load-inherit)

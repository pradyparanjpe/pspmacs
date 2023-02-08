;;; late/init.el --- Late init file, loads modules -*- lexical-binding: t; -*-

(customize-set-variable 'custom-file
  (expand-file-name "custom.el" local-emacs-directory))

(setq gc-cons-threshold (* 2 1000 1000))

(pspmacs/load-modules)

(pspmacs/byte-compile-worktrees)

(pspmacs/load-inherit)

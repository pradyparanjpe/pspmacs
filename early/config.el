;;; early/config.el --- Early config for speedy launch -*- lexical-binding: t; no-byte-compile: t; -*-

(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(global-display-line-numbers-mode 1)
(blink-cursor-mode -1)

(load-theme 'deeper-blue t)

(defalias 'yes-or-no-p 'y-or-n-p)
(setq inhibit-splash-screen nil)
(setq use-file-dialog nil)
(setq tab-bar-new-button-show nil)
(setq tab-bar-close-button-show nil)
(setq tab-line-close-button-show nil)
(setq native-comp-async-report-warnings-errors nil)
(setq byte-compile-warnings
      '(not free-vars unresolved noruntime lexical make-local))

(pspmacs/load-inherit)

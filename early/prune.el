;;; early/prune.el --- Prune overloads -*- lexical-binding: t; no-byte-compile: t; -*-

(setq gc-cons-threshold most-positive-fixnum
      gc-cons-percentage 0.6)

(setq package-enable-at-startup nil
      package-quickstart nil)

(setq inhibit-splash-screen nil)
(setq use-file-dialog nil)
(setq tab-bar-new-button-show nil)
(setq tab-bar-close-button-show nil)
(setq tab-line-close-button-show nil)
(setq native-comp-async-report-warnings-errors nil)
(setq byte-compile-warnings
      '(not free-vars unresolved noruntime lexical make-local))

(dolist
    (prune-mode
     '(menu-bar-mode
       tool-bar-mode
       scroll-bar-mode
       blink-cursor-mode)
     nil)
  (if (fboundp prune-mode) (funcall prune-mode -1)))

(customize-set-variable 'load-prefer-newer t)

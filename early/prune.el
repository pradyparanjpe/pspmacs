;;; early/prune.el --- Prune overloads -*- lexical-binding: t; no-byte-compile: t; -*-

(customize-set-variable 'gc-cons-threshold most-positive-fixnum)
(customize-set-variable 'gc-cons-percentage 0.6)

(customize-set-variable 'package-enable-at-startup nil)
(customize-set-variable 'package-quickstart nil)

(customize-set-variable 'inhibit-splash-screen t)
(customize-set-variable 'use-file-dialog nil)
(customize-set-variable 'tab-bar-new-button-show nil)
(customize-set-variable 'tab-bar-close-button-show nil)
(customize-set-variable 'tab-line-close-button-show nil)
(customize-set-variable 'native-comp-async-report-warnings-errors nil)
(customize-set-variable
 'byte-compile-warnings
 '(not free-vars unresolved noruntime lexical make-local))

(dolist (prune-mode
         '(menu-bar-mode tool-bar-mode scroll-bar-mode blink-cursor-mode))
  (when (fboundp prune-mode) (funcall prune-mode -1)))

(customize-set-variable 'load-prefer-newer t)

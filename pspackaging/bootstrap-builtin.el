;;; bootstrap-builtin.el --- package.el package manager -*- lexical-binding: t; -*-

;; package configuration
(defun pspmacs/init-package-manager ()
  "Initialize `package.el' as the package manager"
  (require 'package)

  ;; Additional package archives
  (add-to-list 'package-archives '("stable" . "https://stable.melpa.org/packages/"))
  (add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))

  (customize-set-variable 'package-archive-priorities
                          '(("gnu"    . 99)
                            ("nongnu" . 80)
                            ("stable" . 70)
                            ("melpa"  . 0)))
  (package-initialize)

  ;; package should store data locally.
  (customize-set-variable 'package-user-dir
                          (expand-file-name "elpa/" local-emacs-directory))
  (unless (file-exists-p package-user-dir)
    (mkdir package-user-dir t))

  (package-refresh-contents))

;;; bootstrap-builtin.el ends here

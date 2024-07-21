(use-package sh-script
  :after eglot
  :config
  (add-to-list
   'eglot-server-programs
   '((sh-mode bash-ts-mode) . ("bash-language-server" "start")))

  :hook
  (sh-mode . eglot-ensure)
  (bash-ts-mode . eglot-ensure))

(use-package bash-completion
  :hook
  (eshell-mode . completion-at-point-functions)
  (eshell-mode . bash-completion-capf-nonexclusive))

(use-package flymake-shell
  :hook
  (sh-set-shell . flymake-shell-load))

(use-package flymake-shellcheck
  :commands flymake-shellcheck-load
  :custom
  (flymake-shellcheck-allow-external-files t)
  :hook
  (sh-mode . flymake-shellcheck-load))

(use-package shfmt
  :general
  (pspmacs/local-leader-keys
    :keymaps 'sh-mode-map
    "Lf" '(:ignore t :wk "ormat")
    "Lff" '(shfmt :wk "script")
    "Lfb" '(shfmt-buffer :wk "uffer"))

  :custom
  (shfmt-arguments '("-i 2" "-mn" "-kp" "-sr" "-ci" "-bn"))

  :hook
  (sh-mode . shfmt-on-save-mode))

(pspmacs/load-inherit)
;;; pspmacs-shell.el ends here

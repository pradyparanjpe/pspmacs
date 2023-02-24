(use-package vterm
  :general
  (pspmacs/leader-keys
    "'" '((lambda () (interactive)
            (pspmacs/inferior-interpreter 'vterm))
           :wk "terminal"))
  :init
  (setq vterm-always-compile-module t
        vterm-ignore-blink-cursor t)
  :config
  (general-add-hook
   'vterm-exit-functions
   '(lambda (_ _) (pspmacs/destroy-buffer-and-window))))

(setq wl-copy-process nil)
(when (string-collate-equalp (getenv "XDG_SESSION_TYPE") "WAYLAND" nil t)
  (setq interprogram-cut-function 'wl-copy)
  (setq interprogram-paste-function 'wl-paste))

(use-package restart-emacs
  :general
  (pspmacs/leader-keys
    "qr" '(restart-emacs :wk "and restart")))

;; Example configuration for Consult
(use-package consult
  ;; Replace bindings. Lazily loaded due by `use-package'.
  :commands (consult-ripgrep)
  :general
  (pspmacs/leader-keys
    "b b" '(consult-buffer :wk "buffer")
    "/" '(consult-ripgrep :wk "find in project")
    "*" '((lambda ()
            (interactive)
            (consult-ripgrep nil (thing-at-point 'symbol)))
          :wk "find this in project")
    "el" '(consult-flymake :wk "list"))

  (pspmacs/local-leader-keys
    "M-x" '(consult-mode-command))
  ;; Enable automatic preview at point in the *Completions* buffer. This is
  ;; relevant when you use the default completion UI.
  :hook (completion-list-mode . consult-preview-at-point-mode)
  :init
  (setq register-preview-delay 0.5
        register-preview-function #'consult-register-format)
  (advice-add #'register-preview :override #'consult-register-window)
  (setq xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref)
  :config
  (consult-customize
   consult-theme :preview-key '(:debounce 0.2 any)
   consult-ripgrep consult-git-grep consult-grep
   consult-bookmark consult-recent-file consult-xref
   consult--source-bookmark consult--source-file-register
   consult--source-recent-file consult--source-project-recent-file
   ;; :preview-key "M-."
   :preview-key '(:debounce 0.4 any))
  (setq consult-narrow-key "<") ;; "C-+"
  (autoload 'projectile-project-root "projectile")
  (setq consult-project-root-function #'projectile-project-root))

(use-package wgrep)

(use-package systemd)

(use-package pass
  :general
  (pspmacs/leader-keys
    "P" 'pass))

(use-package emacs
  :custom
  (epg-pinentry-mode 'loopback)
  (package-gnupghome-dir (expand-file-name "packages/gnupg" local-emacs-directory))
  :config
  (epa-file-enable))

(use-package nginx-mode
  :mode ("/nginx/sites-\\(?:available\\|enabled\\)/" . nginx-mode))

(pspmacs/load-inherit)
;;; pspmacs-os.el ends here

(use-package vterm
  :general
  (pspmacs/leader-keys
    "'" '((lambda () (interactive)
            (pspmacs/inferior-interpreter 'vterm))
           :wk "terminal"))
  :init
  (setq vterm-always-compile-module t
        vterm-ignore-blink-cursor t)
  :ensure t
  :config
  (general-add-hook
   'vterm-exit-functions
   '(lambda (_ _) (pspmacs/destroy-buffer-and-window))))

(setq wl-copy-process nil)
(defun wl-copy (text)
  (setq wl-copy-process (make-process :name "wl-copy"
                  :buffer nil
                  :command '("wl-copy" "-f" "-n")
                  :connection-type 'pipe))
  (process-send-string wl-copy-process text)
  (process-send-eof wl-copy-process))
(defun wl-paste ()
  (if (and wl-copy-process (process-live-p wl-copy-process))
  nil ; should return nil if we're the current paste owner
    (shell-command-to-string "wl-paste -n | tr -d \r")))

(when (string-collate-equalp (getenv "XDG_SESSION_TYPE") "WAYLAND" nil t)
  (setq interprogram-cut-function 'wl-copy)
  (setq interprogram-paste-function 'wl-paste))

(defun pspmacs/yank-file-name ()
  "Yank file-name to clipboard

Also, display file name in echo area"
  (interactive)
  (kill-new buffer-file-name)
  (message (format "Copied: %s"buffer-file-name)))

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
    "*" '(lambda ()
           (interactive)
           (consult-ripgrep nil (thing-at-point 'symbol))
           :kw "find in project"))

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

(use-package systemd)

(use-package nginx-mode
  :mode ("/nginx/sites-\\(?:available\\|enabled\\)/" . nginx-mode))

(pspmacs/load-inherit)

;;; pspmacs-os.el ends here

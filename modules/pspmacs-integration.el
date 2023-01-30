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
  "Yank file-name to clipboard"
  (interactive)
  (kill-new buffer-file-name))

(use-package restart-emacs
  :general
  (pspmacs/leader-keys
    "qr" '(restart-emacs :wk "and restart")))

(use-package helm-ag
  :general
  (pspmacs/leader-keys
    "/" '(lambda ()
           (interactive)
           (helm-do-ag (or projectile-project-root default-directory)))
    :kw "find in project"
    "*" '(lambda ()
           (interactive)
           (helm-do-ag (or projectile-project-root default-directory) nil
                       (thing-at-point 'symbol)))
    :kw "find in project")
  :init
  (cond
   ((executable-find "rg")
    (custom-set-variables
     '(helm-ag-base-command "rg --no-heading")
     `(helm-ag-success-exit-status '(0 2))))
   ((executable "pt")
    (custom-set-variables
     '(helm-ag-base-command "pt -e --nocolor --nogroup")))
   ((executable "ack")
    (custom-set-variables
     '(helm-ag-base-command "ack --nocolor --nogroup")))))

(use-package systemd)

(use-package nginx-mode
  :mode ("/nginx/sites-\\(?:available\\|enabled\\)/" . nginx-mode))

(pspmacs/load-inherit)

;;; pspmacs-os.el ends here

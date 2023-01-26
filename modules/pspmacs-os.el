(use-package vterm
  :general
  (pspmacs/leader-keys
    "'" '((lambda () (interactive)
            (let ((term-window (split-window-below)))
              (select-window term-window)
              (vterm)))
          :wk "terminal"))
  :init
  (setq vterm-always-compile-module t
        vterm-ignore-blink-cursor t)
  :ensure t
  :config
  (general-add-hook
   'vterm-exit-functions
   '(lambda (_ _) (pspmacs/destroy-buffer-and-window))))

(pspmacs/load-inherit)
(provide 'pspmacs-os)
;;; os.el ends here

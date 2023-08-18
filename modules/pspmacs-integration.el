;;; pspmacs-integration.el --- User experience/interface -*- lexical-binding: t; -*-

;; Copyright © 2023  Pradyumna Swanand Paranjape

;; Author: Pradyumna Swanand Paranjape <pradyparanjpe@rediffmail.com>
;; Keywords: help, languages

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU Lesser General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU Lesser General Public License for more details.

;; You should have received a copy of the GNU Lesser General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;;; Code:

(use-package vterm
  :general
  (pspmacs/leader-keys
    "'v" '((lambda () (interactive)
             (pspmacs/inferior-interpreter 'vterm))
           :wk "vterm"))
  :custom
  (vterm-always-compile-module t)
  (vterm-ignore-blink-cursor t)
  :config
  (general-add-hook
   'vterm-exit-functions
   '(lambda (&rest _) (pspmacs/destroy-buffer-and-window))))

(use-package eat
  :general
  (pspmacs/leader-keys
    "'e" '((lambda () (interactive)
             (pspmacs/inferior-interpreter 'eat))
           :wk "eat"))
  :config
  (general-add-hook
     'eat-exit-hook
     '(lambda (&rest _) (pspmacs/destroy-buffer-and-window))))

(setq wl-copy-process nil)
(when (string-collate-equalp (getenv "XDG_SESSION_TYPE") "WAYLAND" nil t)
  (setq interprogram-cut-function 'wl-copy)
  (setq interprogram-paste-function 'wl-paste))

(use-package consult
  :commands (consult-ripgrep)
  :general
  (pspmacs/leader-keys
    "/" '(consult-ripgrep :wk "find: proj")
    "*" '((lambda ()
            (interactive)
            (consult-ripgrep nil (thing-at-point 'symbol)))
          :wk "/'THIS'")
    "Br" '(consult-bookmark :wk "remember")
    "bb" '(consult-buffer :wk "menu")
    "el" '(consult-flymake :wk "list")
    "fc" '(consult-find :wk "consult")
    "fr" '(consult-recent-file :wk "recent")
    "mj" '(consult-mark :wk "jump")
    "rl" '(consult-register-load t :wk "load")
    "rr" '(consult-register-store :wk "remember")
    "rj" '(consult-register :wk "jump"))

  (pspmacs/local-leader-keys
    "M-x" '(consult-mode-command :wk "mode command"))
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
  (autoload 'projectile-project-root "projectile")
  (add-to-list 'consult-buffer-filter "\\`\\*epc con [0-9]+\\*\\'")
  :custom
  (consult-narrow-key "<") ;; "C-+"
  (consult-project-root-function #'projectile-project-root))

(use-package wgrep)

(use-package systemd)

(use-package pass)

(use-package emacs
  :custom
  (epg-pinentry-mode 'loopback)
  (package-gnupghome-dir (expand-file-name "packages/gnupg" local-emacs-dir))
  :config
  (epa-file-enable))

(pspmacs/load-inherit)
;;; pspmacs-os.el ends here

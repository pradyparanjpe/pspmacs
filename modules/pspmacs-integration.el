;;; pspmacs-integration.el --- User experience/interface -*- lexical-binding: t; -*-

;; Copyright © 2023-2024  Pradyumna Swanand Paranjape

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
    "'v" '(vterm :wk "vterm"))
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
    "'e" '(eat :wk "eat"))
  :config
  (general-add-hook
     'eat-exit-hook
     '(lambda (&rest _) (pspmacs/destroy-buffer-and-window))))

(use-package eshell
    :general
    (pspmacs/leader-keys
      "'E" '(eat :wk "eshell"))
    :config
    (general-add-hook
       'eat-exit-hook
       '(lambda (&rest _) (pspmacs/destroy-buffer-and-window))))

(use-package exec-path-from-shell
  :custom
  (exec-path-from-shell-arguments nil)
  (dolist (envvar '("SSH_AUTH_SOCK" "SSH_AGENT_PID" "GPG_AGENT_INFO" "LC_CTYPE"
                    "LANG" "NIX_SSL_CERT_FILE" "NIX_PATH" "PKG_CONFIG_PATH"))
    (add-to-list 'exec-path-from-shell-variables envvar))
  :init
  (when (or (daemonp)
            (memq window-system '(mac ns x)) (string= system-type "darwin"))
    (exec-path-from-shell-initialize)))

(when (string-collate-equalp (getenv "XDG_SESSION_TYPE") "WAYLAND" nil t)
  (setq interprogram-cut-function 'wl-copy)
  (setq interprogram-paste-function 'wl-paste))

(use-package consult
  :commands (consult-ripgrep)
  :general
  (pspmacs/leader-keys
    "/" '(consult-ripgrep :wk "🔍📁")
    "*" '((lambda ()
            (interactive)
            (consult-ripgrep nil (thing-at-point 'symbol)))
          :wk "/'THIS'")
    "Br" '(consult-bookmark :wk "remember")
    "bb" '(consult-buffer :wk "𑂼")
    "el" '(consult-flymake :wk "list")
    "fc" '(consult-find :wk "consult")
    "fr" '(consult-recent-file :wk "recent")
    "js" '(consult-imenu :wk "section")
    "jS" '(consult-imenu-multi :wk "project section")
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
  (add-to-list 'consult-buffer-filter "\\`\\*epc con [0-9]+\\*\\'")
  (add-to-list 'consult-buffer-filter "\\`\\*EGLOT .+ events\\*\\'")
  :custom
  ;; "C-+"
  (consult-narrow-key "<"))

(use-package wgrep)

(use-package pass
  :custom
  (auth-sources '("~/.authinfo.gpg" "~/.authinfo" "~/.netrc.gpg" "~/.netrc"))
  :init (auth-source-pass-enable))

(use-package pinentry
  :custom
  (epg-pinentry-mode 'loopback)
  (package-gnupghome-dir (expand-file-name "packages/gnupg" local-emacs-dir))
  (pinentry--socket-dir (xdg/make-path (format "pinentry") 'state))
  :config
  (epa-file-enable)
  (unless pinentry--server-process
    (pinentry-start)))

(use-package emacs
  :custom
  (async-byte-compile-log-file (xdg/make-path "async-bytecomp.log" 'state)))

(pspmacs/load-inherit)
;;; pspmacs-os.el ends here

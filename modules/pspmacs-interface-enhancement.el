;;; pspmacs-interface-enhancement.el --- User experience/interface -*- lexical-binding: t; -*-

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

;;; Garbage collector
(use-package gcmh
  :demand t
  :config
  (gcmh-mode 1))

(use-package general
  :demand t
  :config
  (general-evil-setup)
  ;; integrate general with evil

  ;; set up 'SPC' as the global leader key
  (general-create-definer pspmacs/leader-keys
    :states '(normal insert visual emacs)
    :keymaps 'override
    :prefix "SPC" ;; set leader
    :global-prefix "M-SPC") ;; access leader in insert mode

  ;; set up ',' as the local leader key
  (general-create-definer pspmacs/local-leader-keys
    :states '(normal insert visual emacs)
    :keymaps 'override
    :prefix "," ;; set local leader
    :global-prefix "M-,") ;; access local leader in insert mode

  ;; don't stretch for ESC
  ;; (general-define-key
  ;;  :states 'insert
  ;;  "jk" 'evil-normal-state)
  (general-imap "j"
    (general-key-dispatch 'self-insert-command
      "k" 'evil-normal-state))
  (general-rmap "j"
    (general-key-dispatch 'self-insert-command
      "k" 'evil-normal-state))

  ;; unbind some annoying default bindings
  (general-unbind
    "C-x C-z"   ;; unbind suspend frame
    "C-x C-d"   ;; unbind list directory
    "<mouse-2>") ;; pasting with mouse wheel click

  (general-define-key "ESC" #'keyboard-escape-quit)

  ;; rebind C-c C-c to ,,
  (general-nmap ",," (general-simulate-key "C-c C-c"))

  (pspmacs/leader-keys
    :states 'normal
    "+" '(:ignore t :wk "Emacs-Repo Update")
    "+m" '(emacs-repo :wk "Transient menu")
    "++" '(emacs-repo/repo-install :wk "Upgrade with defaults"))

  (pspmacs/leader-keys
    "TAB" '((lambda ()
                (interactive)
                (switch-to-buffer (other-buffer (current-buffer) 1)))
              :which-key "toggle buffer"))

  (pspmacs/leader-keys
    "1" '(:ignore t :wk "line-numbers")
    "1d" '((lambda () (interactive) (setq display-line-numbers nil))
      :wk "disable")
    "1e" '((lambda () (interactive) (setq display-line-numbers t))
      :wk "enable")
    "1r" '((lambda () (interactive) (setq display-line-numbers 'relative))
      :wk "relative")
    "1v" '((lambda () (interactive) (setq display-line-numbers 'visual))
      :wk "visual"))

  (pspmacs/leader-keys
    "8" '(insert-char :wk "UTF-8 character"))

  ;; buffer
  ;; see 'bufler' and 'popper'
  (pspmacs/leader-keys
    "b" '(:ignore t :wk "buffer")
    "bd" '(kill-this-buffer :wk "kill this buffer")
    "b TAB" '((lambda ()
                (interactive)
                (switch-to-buffer (other-buffer (current-buffer) 1)))
              :which-key "toggle")
    "bm" '((lambda ()
         (interactive) (switch-to-buffer (get-buffer-create "*Messages*")))
       :which-key "messages")
    "bn" '(next-buffer :wk "previous buffer")
    "bp" '(previous-buffer :wk "previous buffer")
    "bs" '((lambda () (interactive) (switch-to-buffer "*scratch*"))
       :wk "scratch")
    "br" '(revert-buffer :wk "reload buffer")
    "bw" '(read-only-mode :wk "read-only")
    "b C-d" '(pspmacs/kill-other-buffers :wk "delete other"))

  ;; bookmark
  (pspmacs/leader-keys
    "B" '(:ignore t :wk "bookmark")
    "Bs" '(bookmark-set :wk "set bookmark")
    "Bj" '(bookmark-jump :wk "jump to bookmark"))

  (pspmacs/leader-keys
    "c" '(:ignore t :wk "comment")
    "cl" '(comment-line :wk "comment line")
    "cp" '(comment-region :wk "comment paragraph"))

  ;; worktrees
  (pspmacs/leader-keys
    "f" '(:ignore t :wk "file")
    "ff" '(find-file :wk "find")
    "fe" '(:ignote t :wk "emacs")
    "fec" '((lambda ()
      (interactive)
      (find-file custom-file))
            :wk "custom file")
    "fw" '(:ignote t :wk "worktree")
    "fwl" '((lambda ()
              (interactive)
              (find-file local-emacs-directory))
            :wk "local")
    "fwp" '((lambda ()
              (interactive)
              (if pvt-emacs-directory
                  (find-file pvt-emacs-directory)
                (message "private work-tree is not declared.")))
            :wk "private")
    "fwg" '((lambda ()
              (interactive)
              (message "disabled"))
            :wk "global <disabled>")
    "fD" '((lambda ()
         (interactive)
         (delete-file (buffer-file-name)))
       :wk "delete File")
    "fR" '(rename-file :wk "rename")
    "fs" '(save-buffer :wk "save file")
    "fS" '(write-file :wk "save as")
    "fy" '(pspmacs/yank-file-name :wk "Copy file name"))

  ;; help
  (pspmacs/leader-keys "h" (general-simulate-key "C-h"))

  ;; major mode
  (pspmacs/leader-keys
    "M" '(:ignore t :wk "Major Mode")
    "Me" 'emacs-lisp-mode
    "Mo" 'org-mode
    "Mp" 'python-mode
    "Mr" 'ess-r-mode
    "Ms" 'shell-script-mode
    "M-" 'fundamental-mode)

  ;; universal argument
  (pspmacs/leader-keys
    "u" '(universal-argument :wk "universal prefix"))

  ;; notes
  ;; see 'citar' and 'org-roam'
  (pspmacs/leader-keys
    "n" '(:ignore t :wk "notes"))

  ;; see org-roam and citar sections

  ;; code
  ;; see 'flymake'
  (pspmacs/leader-keys
    "c" '(:ignore t :wk "code"))

  ;; open
  (pspmacs/leader-keys
    "o" '(:ignore t :wk "open")
    "os" '(speedbar t :wk "speedbar"))

  (pspmacs/leader-keys
    "q" '(:ignore t :wk "quit")
    ;; see org-roam and citar sections
    "qQ" '(kill-emacs :wk "daemon")
    "q C-f" '(delete-other-frames :wk "other frames")
    "qq" '(delete-frame :wk "client"))

  ;; templating
  ;; see 'tempel'
  (pspmacs/leader-keys
    "t" '(:ignore t :wk "template"))

  (pspmacs/leader-keys
    "w" '(:ignore t :which-key "window")
    "wr" 'winner-redo
    "w=" 'balance-windows-area
    "wD" 'kill-buffer-and-window
    "w C-d" '(delete-other-windows :wk "delete other")))

(use-package dashboard
  :demand t
  :diminish dashboard-mode
  :general
  (pspmacs/leader-keys
    "bh" '(pspmacs-dashboard :which-key "home splash"))
  :init
  (defun pspmacs-dashboard ()
    (interactive)
    (pspmacs/home-splash)
    (switch-to-buffer
     (get-buffer-create "*dashboard*")))
  :custom
  (dashboard-image-banner-max-width 300)
  (dashboard-startup-banner
    (expand-file-name "data/Tux.png" user-emacs-directory))
  (dashboard-set-heading-icons t)
  (dashboard-banner-logo-title
    "Prady's Structured, Personalized Emacs")
  (dashboard-items '((projects . 2)
                     (recents . 5)
                     (agenda . 5)))
  (dashboard-center-content t)
  (dashboard-set-footer nil)
  (initial-buffer-choice
   (lambda ()
     (switch-to-buffer
      (get-buffer-create "*dashboard*"))))
  :config
  (dashboard-setup-startup-hook)
  :hook
  (dashboard-after-initialize-hook . pspmacs/home-splash))

(use-package helpful
  :after evil
  :init
  (setq evil-lookup-func #'helpful-at-point)
  :bind
  ([remap describe-function] . helpful-callable)
  ([remap describe-command] . helpful-command)
  ([remap describe-variable] . helpful-variable)
  ([remap describe-key] . helpful-key))

(use-package ace-popup-menu
  :custom
  (ace-popup-menu-show-pane-header t)
  (ace-popup-menu-mode 1))

(use-package avy
  :general
  (pspmacs/leader-keys
    "j" '(:ignore t :wk "jump")
    "jj" '(avy-goto-char-timer :wk "search")))

(use-package evil
  :general
  ;; window navigations
  (pspmacs/leader-keys
    "w" '(:ignore t :keymap evil-window-map :wk "window") ;; window bindings
    "wd" '(evil-window-delete :wk "delete window")
    "wj" '(evil-window-down :wk "down window")
    "wk" '(evil-window-up :wk "up window")
    "wl" '(evil-window-left :wk "left window")
    "wn" '(evil-window-next :wk "next window")
    "wp" '(evil-window-prev :wk "previous window")
    "wr" '(evil-window-right :wk "right window")
    "ws" '(evil-window-split :wk "split window horizontally")
    "wv" '(evil-window-vsplit :wk "split window vertically"))
  (general-define-key :keymaps 'evil-motion-state-map "RET" nil)
  (general-define-key :keymaps 'evil-insert-state-map "C-k" nil)
  :demand t
  :init
  (setq
   ;; allow scroll up with 'C-u'
   evil-want-C-u-scroll t
   ;; allow scroll down with 'C-d'
   evil-want-C-d-scroll t
   ;; necessary for evil collection
   evil-want-integration t
   evil-want-keybinding nil
   ;; hopefully this will fix weird tab behaviour
   evil-want-C-i-jump nil)
  :config
  (setq evil-search-module 'isearch
        evil-split-window-below t
        evil-vsplit-window-right t
        evil-undo-system 'undo-tree
        evil-normal-state-cursor '(box "orange")
        evil-insert-state-cursor '((bar . 3) "green")
        evil-visual-state-cursor '(box "light blue")
        evil-replace-state-cursor '(box "yellow"))

  (evil-mode t) ;; globally enable evil mode
  ;; default mode: normal
  (evil-set-initial-state 'messages-buffer-mode 'normal)
  (evil-set-initial-state 'dashboard-mode 'normal)
  ;; default mode: insert
  (evil-set-initial-state 'eshell-mode 'insert)
  (evil-set-initial-state 'magit-diff-mode 'insert))

(use-package evil-collection ;; evilifies a bunch of things
  :after evil
  :demand t
  :custom
  ;; '<TAB>' cycles visibility in 'outline-minor-mode'
  (evil-collection-outline-bind-tab-p t)
  (evil-collection-setup-minibuffer t)
  :config
  (evil-collection-init))

(use-package evil-surround
  :demand t
  :after evil
  :hook
  ((org-mode . (lambda () (push '(?~ . ("~" . "~")) evil-surround-pairs-alist)))
   (org-mode . (lambda () (push '(?$ . ("\\(" . "\\)")) evil-surround-pairs-alist))))
  :config
  (global-evil-surround-mode 1))

(use-package evil-goggles
  :demand t
  :config
  (evil-goggles-mode)
  (evil-goggles-use-diff-faces))

;; Enable vertico
(use-package vertico
  :demand t
  :general
  (:keymaps 'vertico-map
            "C-j" #'vertico-next
            "C-k" #'vertico-previous
            "<escape>" #'minibuffer-keyboard-quit ; Close minibuffer
            ;; "C-;" #'kb/vertico-multiform-flat-toggle
            "M-<backspace>" #'vertico-directory-delete-word)
  (pspmacs/leader-keys
    "SPC" '(execute-extended-command :wk "vertico M-x"))
  :init
  (vertico-mode))

;; Persist history over Emacs restarts. Vertico sorts by history position.

(use-package savehist
  :init
  (savehist-mode))

(use-package marginalia
  ;; Either bind `marginalia-cycle' globally or only in the minibuffer
  :after vertico
  :demand t
  :general
  (general-define-key
   :keymaps 'minibuffer-local-map
   "M-a" #'marginalia-cycle)
  :init
  (marginalia-mode))

(use-package orderless
  :after vertico
  :demand t
  :init
  ;; Configure a custom style dispatcher (see the Consult wiki)
  ;; (setq orderless-style-dispatchers '(+orderless-dispatch)
  ;;       orderless-component-separator #'orderless-escapable-split-on-space)
  (setq completion-styles '(orderless partial-completion basic)
        completion-category-defaults nil)
  (add-to-list 'completion-category-overrides '(eglot orderless)))

;; Configure Tempel
(use-package tempel
  ;; Require trigger prefix before template name when completing.
  ;; :custom
  ;; (tempel-trigger-prefix "<")

  :general
  (pspmacs/leader-keys
    (">" '(:ignore t :wk "tempel templates"))
    (">>" '(tempel-complete :wk "complete"))
    (">i" '(tempel-insert :wk "insert")))

  :init
  ;; Setup completion at point
  (defun tempel-setup-capf ()
    ;; Add the Tempel Capf to `completion-at-point-functions'.
    ;; `tempel-expand' only triggers on exact matches. Alternatively use
    ;; `tempel-complete' if you want to see all matches, but then you
    ;; should also configure `tempel-trigger-prefix', such that Tempel
    ;; does not trigger too often when you don't expect it. NOTE: We add
    ;; `tempel-expand' *before* the main programming mode Capf, such
    ;; that it will be tried first.
    (setq-local completion-at-point-functions
                (cons #'tempel-expand
                      completion-at-point-functions)))

  :hook
  ((prog-mode text-mod) . tempel-setup-capf)

  ;; Optionally make the Tempel templates available to Abbrev,
  ;; either locally or globally. `expand-abbrev' is bound to C-x '.
  ;; (add-hook 'prog-mode-hook #'tempel-abbrev-mode)
  ;; (global-tempel-abbrev-mode)
)

;; Optional: Add tempel-collection.
;; The package is young and doesn't have comprehensive coverage.
(use-package tempel-collection)

(use-package embark
  :after vertico
  :general
  (general-def
    "C-`" 'embark-act
    "C-~" 'embark-export)
  :demand t
  :config
  ;; Hide the mode line of the Embark live/completions buffers
  (add-to-list 'display-buffer-alist
               '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                 nil
                 (window-parameters (mode-line-format . none)))))

;; Consult users will also want the embark-consult package.
(use-package embark-consult
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))

(use-package doom-modeline
  :demand t
  :init
  (setq display-time-24hr-format t)
  (display-time-mode)
  (setq display-time-default-load-average nil)
  (line-number-mode t)
  (column-number-mode t)
  (size-indication-mode t)
  :custom
  (doom-modeline-icon t)
  (doom-modeline-modal-icon "")
  (doom-modeline-major-mode-icon t)
  (doom-modeline-major-mode-color-icon t)
  (doom-modeline-env-version t)
  (doom-modeline-buffer-file-name-style 'relative-to-project)
  (doom-modeline-buffer-encoding nil)
  (doom-modeline-height 15)
  (doom-modeline-project-detection 'projectile)
  :config
  ;; with emacs-29 on doom-modeline release, following issue *still* persists
  ;; https://github.com/seagle0128/doom-modeline/issues/505
  ;; workaround:
  (unless (version< emacs-version "29")
    (setq doom-modeline-fn-alist
          (--map
           (cons (remove-pos-from-symbol (car it)) (cdr it))
           doom-modeline-fn-alist)))
  (doom-modeline-mode 1)
  (set-face-attribute 'mode-line nil
                      :background "#050614"
                      :foreground "white"
                      :box '(:line-width 8 :color "#050614")
                      :overline nil
                      :underline nil)
  (set-face-attribute 'mode-line-inactive nil
                      :background "#262033"
                      :foreground "white"
                      :box '(:line-width 8 :color "#262033")
                      :overline nil
                      :underline nil)
  (set-face-attribute 'doom-modeline-buffer-file nil
                      :foreground "#009f9f")
  (set-face-attribute 'doom-modeline-time nil
                      :foreground "#9fafbf")
  (set-face-attribute 'doom-modeline-evil-insert-state nil
                      :foreground "green")
  (set-face-attribute 'doom-modeline-evil-normal-state nil
                      :foreground "orange")
  (set-face-attribute 'doom-modeline-evil-replace-state nil
                      :foreground "yellow")
  (set-face-attribute 'doom-modeline-evil-visual-state nil
                      :foreground "cyan"))

(use-package yascroll
  :custom
  (global-yascroll-bar-mode t)
  (yascroll-delay-to-hide 2.0)
  :config
  (set-face-attribute 'yascroll:thumb-fringe nil
                      :background "#7f7f99"
                      :foreground "#7f7f99")
  (set-face-attribute 'yascroll:thumb-text-area nil
                      :background "#7f7f99"))

(use-package solaire-mode
  :config
  (solaire-global-mode +1))

(use-package hl-todo
  :demand t
  :custom
  (hl-todo-keyword-faces pspmacs/hl-tag-faces)
  :config
  (global-hl-todo-mode))

(use-package emacs
  :init
  ;; Vertico suggestions
  ;; Add prompt indicator to `completing-read-multiple'.
  ;; We display [CRM<separator>], e.g., [CRM,] if the separator is a comma.
  (defun crm-indicator (args)
    (cons (format "[CRM%s] %s"
                  (replace-regexp-in-string
                   "\\`\\[.*?]\\*\\|\\[.*?]\\*\\'" ""
                   crm-separator)
                  (car args))
          (cdr args)))
  (advice-add #'completing-read-multiple :filter-args #'crm-indicator)

  ;; Vertico suggestions
  ;; Do not allow the cursor in the minibuffer prompt
  (setq minibuffer-prompt-properties
        '(read-only t cursor-intangible t face minibuffer-prompt))
  (add-hook 'minibuffer-setup-hook #'cursor-intangible-mode)

  ;; Vertico suggestions
  ;; Emacs 28: Hide commands in M-x which do not work in the current mode.
  ;; Vertico commands are hidden in normal buffers.
  (setq read-extended-command-predicate
        #'command-completion-default-include-p)

  ;; Vertico suggestions
  ;; Enable recursive minibuffers
  (setq enable-recursive-minibuffers t)

  ;;; locale
  (setq locale-coding-system 'utf-8)
  (setq coding-system-for-read 'utf-8)
  (setq coding-system-for-write 'utf-8)
  (setq default-process-coding-system '(utf-8-unix . utf-8-unix))
  (set-terminal-coding-system 'utf-8)
  (set-keyboard-coding-system 'utf-8)
  (set-selection-coding-system 'utf-8)
  (prefer-coding-system 'utf-8)
  (set-default-coding-systems 'utf-8)

  ;;; Font
  (if (daemonp)
      (add-hook 'after-make-frame-functions
                (lambda (frame)
                  (with-selected-frame frame
                    (pspmacs/set-font-faces))))
    (pspmacs/set-font-faces))

  (global-set-key (kbd "C-=") 'text-scale-increase)
  (global-set-key (kbd "C--") 'text-scale-decrease)

  ;;; scroll
  (setq scroll-margin 5)

  ;; tabs
  (setq-default indent-tabs-mode nil
                tab-width 4)

  ;; svg cache
  (setq svg-lib-icons-dir
        (expand-file-name "svg-lib" xdg/emacs-cache-directory))

  ;; Use dialog boxes
  (setq use-dialog-box nil))

(pspmacs/load-inherit)
;;; pspmacs-interface-enhancement.el ends here

;;; pspmacs-interface-enhancement.el --- User experience/interface -*- lexical-binding: t; -*-

;; Copyright (C) 2023  Pradyumna Swanand Paranjape

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

(global-set-key (kbd "<escape>") 'keyboard-escape-quit)

(use-package general
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

  ;; rebind C-c C-c to ,,
  (general-nmap ",," (general-simulate-key "C-c C-c"))

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
    "TAB" '(:keymap tab-prefix-map :wk "tab")) ;; remap tab bindings

  ;; buffer 
  ;; see 'bufler' and 'popper'
  (pspmacs/leader-keys
    "b" '(:ignore t :wk "buffer")
    "b TAB" '((lambda ()
        (interactive)
        (switch-to-buffer (other-buffer (current-buffer) 1)))
      :which-key "last buffer")
    "bd" '(kill-this-buffer :wk "kill this buffer")
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

  ;; file
  (pspmacs/leader-keys
    "f" '(:ignore t :wk "file")
    "fe" '(:ignote t :wk "emacs")
    "fec" '((lambda ()
      (interactive)
      (find-file custom-file))
        :wk "custom file")
    "fel" '((lambda ()
      (interactive)
      (find-file local-emacs-directory))
        :wk "local directory")
    "fep" '((lambda ()
      (interactive)
      (find-file pvt-emacs-directory))
        :wk "private directory")
    "fD" '((lambda ()
         (interactive)
         (delete-file (buffer-file-name)))
       :wk "delete File")
    "fR" '(rename-file :wk "rename")
    "fs" '(save-buffer :wk "save file")
    "fy" '(pspmacs/yank-file-name :wk "Copy file name"))

  ;; help
  ;; namespace mostly used by 'helpful'
  (pspmacs/leader-keys "h" (general-simulate-key "C-h"))

  ;; universal argument
  (pspmacs/leader-keys
    "u" '(universal-argument :wk "universal prefix"))

  ;; notes
  ;; see 'citar' and 'org-roam'
  (pspmacs/leader-keys
    "n" '(:ignore t :wk "notes")
    ;; see org-roam and citar sections
    "na" '(org-todo-list :wk "agenda todos")) ;; agenda

  ;; code
  ;; see 'flymake'
  (pspmacs/leader-keys
    "c" '(:ignore t :wk "code"))

  ;; open
  (pspmacs/leader-keys
    "o" '(:ignore t :wk "open")
    "os" '(speedbar t :wk "speedbar")) ;; TODO this needs some love

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

;; "c" '(org-capture :wk "capture")))

(use-package dashboard
  :demand t
  :diminish dashboard-mode
  :general
  (pspmacs/leader-keys
    "bh" '(pspmacs/home-splash :which-key "home splash"))
  :init
  (setq dashboard-set-footer nil)
  (defun pspmacs/home-splash ()
    "Visit home screen"
    (interactive)
    (progn
  (switch-to-buffer (get-buffer-create "*dashboard*"))
  (dashboard-refresh-buffer)))
  :config
  (setq dashboard-startup-banner
    (expand-file-name "data/Tux.svg" user-emacs-directory))
  (setq dashboard-banner-logo-title
    "Prady' Structured, Personalized Emacs")
  (setq dashboard-items '((projects . 2)
              (recents . 5)
              (agenda . 5)))
  (setq dashboard-center-content t)
  (dashboard-setup-startup-hook)
  (setq initial-buffer-choice (lambda () (pspmacs/home-splash))))

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
  :init
  (setq ace-popup-menu-show-pane-header t)
  :config
  (ace-popup-menu-mode 1))

(use-package avy
  :general
  (pspmacs/leader-keys
    "j" '(:ignore t :wk "jump")
    "jj" '(avy-goto-char-timer :wk "search")))

(use-package evil
  :general
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

  :init
  (setq evil-search-module 'isearch)
  (setq evil-want-C-u-scroll t) ;; allow scroll up with 'C-u'
  (setq evil-want-C-d-scroll t) ;; allow scroll down with 'C-d'
  (setq evil-want-integration t) ;; necessary for evil collection
  (setq evil-want-keybinding nil)
  (setq evil-split-window-below t)
  (setq evil-vsplit-window-right t)
  (setq evil-want-C-i-jump nil) ;; hopefully this will fix weird tab behaviour
  (setq evil-undo-system 'undo-redo) ;; undo via 'u', and redo the undone change via 'C-r'; only available in emacs 28+.
  (setq evil-normal-state-cursor '(box "orange"))
  (setq evil-insert-state-cursor '((bar . 3) "green"))
  (setq evil-visual-state-cursor '(box "light blue"))
  (setq evil-replace-state-cursor '(box "yellow"))
  :config
  (evil-mode t) ;; globally enable evil mode
  ;; set the initial state for some kinds of buffers.
  (evil-set-initial-state 'messages-buffer-mode 'normal)
  (evil-set-initial-state 'dashboard-mode 'normal)
  ;; buffers in which I want to immediately start typing should be in 'insert' state by default.
  (evil-set-initial-state 'eshell-mode 'insert)
  (evil-set-initial-state 'magit-diff-mode 'insert))

(use-package evil-collection ;; evilifies a bunch of things
  :after evil
  :init
  (setq evil-collection-outline-bind-tab-p t) ;; '<TAB>' cycles visibility in 'outline-minor-mode'

  (setq evil-collection-setup-minibuffer t)
  :config
  (evil-collection-init))

(use-package evil-surround
  :after evil
  :hook ((org-mode . (lambda () (push '(?~ . ("~" . "~")) evil-surround-pairs-alist)))
     (org-mode . (lambda () (push '(?$ . ("\\(" . "\\)")) evil-surround-pairs-alist))))
  :config
  (global-evil-surround-mode 1))

(use-package evil-goggles
  :config
  (evil-goggles-mode)
  (evil-goggles-use-diff-faces))

(use-package helm
  :demand t
  :bind (("M-x" . helm-M-x))
  :general
  (general-define-key
   :keymaps 'helm-map
   "TAB" #'helm-execute-persistent-action
   "C-z" #'helm-select-action)
  (pspmacs/leader-keys
    "SPC" '(helm-M-x :wk "helm-M-x"))
  (pspmacs/leader-keys
    "ff" '(helm-find-files :wk "find files")
    "bb" '(helm-buffers-list :wk "switch buffer"))
  :config
  (helm-mode 1)
  (setq read-extended-command-predicate
        #'command-completion-default-include-p))

(use-package doom-modeline
  :demand t
  :init
  (setq doom-modeline-env-version t)
  (setq doom-modeline-buffer-encoding nil)
  (setq doom-modeline-height 15)
  (setq doom-modeline-project-detection 'projectile)
  (setq doom-modeline-icon t)
  :config
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
  (set-face-attribute 'doom-modeline-evil-insert-state nil
          :foreground "green")
  (set-face-attribute 'doom-modeline-evil-normal-state nil
          :foreground "orange")
  (set-face-attribute 'doom-modeline-evil-replace-state nil
          :foreground "yellow")
  (set-face-attribute 'doom-modeline-evil-visual-state nil
          :foreground "cyan"))

(use-package solaire-mode
  :config
  (solaire-global-mode +1))

(defun pspmacs/kill-other-buffers ()
  "Kill all other buffers."
  (interactive)
  (mapc 'kill-buffer (delq (current-buffer) (buffer-list))))

(defun pspmacs/destroy-buffer-and-window (&optional target-buffer)
  "Destroy window and buffer after some process is done

If TARGET-BUFFER is supplied, it and its window is destroyed.
Else, current buffer and window is destroyed.
If window is the only window, it is spared"
  (let* ((used-buffer (or target-buffer (current-buffer)))
         (used-window (get-buffer-window used-buffer)))
    (when (not (one-window-p))
      (delete-window used-window))
    (kill-buffer used-buffer)))

(defun pspmacs/extend-list (list-var elements)
  "Iterative form of ‘add-to-list’.

Return value is the new value of LIST-VAR"
  (unless (consp elements)
    (error "ELEMENTS must be list"))
  (dolist (elem elements)
    (add-to-list list-var elem))
  (symbol-value list-var))

(use-package hl-todo
  :init
  (setq hl-todo-keyword-faces
    '(("FAIL"  .  "#ff3f3f")
      ("FIXME" .  "#ff6f3f")
      ("TEMP"  .  "#ff9f3f")
      ("HACK"  .  "#ffcf3f")
      ("TODO"  .  "#ffff3f")
      ("LAZY"  .  "#e7ff3f")
      ("WAIT"  .  "#cfff3f")
      ("NEXT"  .  "#9fff3f")
      ("ALGO"  .  "#6fff3f")
      ("PROG"  .  "#3fff3f")
      ("TEST"  .  "#3fe757")
      ("ACTS"  .  "#3fcf6f")
      ("SENT"  .  "#3f9f9f")
      ("OKAY"  .  "#3f6fcf")
      ("DONE"  .  "#3f3fff")
      ("NOTE"  .  "#ffcf6f")
      ("XXXX"  .  "#ff9f9f")
      ("DONT"  .  "#ff6fcf")
      ("CANT"  .  "#ff3fff")))
  (global-hl-todo-mode))

(use-package emacs
  :init
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

  ;;; UI
  (setq display-time-24hr-format t)
  (customize-set-variable 'large-file-warning-threshold (* 100 1000 1000))
  (global-hl-line-mode 1)
  (column-number-mode t)
  (display-fill-column-indicator-mode)
  (display-time-mode)

  ;;; Font
  (set-face-attribute 'default nil :font "Fira Code" :height 150)
  (global-set-key (kbd "C-=") 'text-scale-increase)
  (global-set-key (kbd "C--") 'text-scale-decrease)

  ;;; auto-complete
  ;; tabs
  (setq-default indent-tabs-mode nil
        tab-width 4))

(pspmacs/load-inherit)

;;; pspmacs-interface-enhancement.el ends here

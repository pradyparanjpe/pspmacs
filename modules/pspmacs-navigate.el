;;; pspmacs-navigate.el --- navigate intra/extra-file -*- lexical-binding: t; -*-

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

(use-package multiple-cursors
  :after evil
  :ensure t
  :general
  (pspmacs/leader-keys
    "s" '(:ignore t :wk "multiple-substitute")
    "se" '(:ignore t :state 'visual :wk "edit")
    :states 'visual
    "sef" '(mc/mark-all-like-this-in-defun :wk "function")
    "seb" '(mc/mark-all-like-this :wk "buffer"))
  (pspmacs/leader-keys
    :states 'normal
    "sef" '(mc/mark-all-symbols-like-this-in-defun :wk "function")
    "seb" '(mc/mark-all-symbols-like-this :wk "buffer")))

(use-package avy
  :general
  (pspmacs/leader-keys
    "j" '(:ignore t :wk "jump")
    "jj" '(avy-goto-char-timer :wk "search")))

(use-package helm-ag
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
 '(helm-ag-base-command "ack --nocolor --nogroup"))))

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

  :config
  (setq treemacs-no-png-images t treemacs-width 24)
  :bind ("C-c t" . treemacs))

(pspmacs/load-inherit)
(provide 'pspmacs-navigate)

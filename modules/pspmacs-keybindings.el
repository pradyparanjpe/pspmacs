;;; pspmacs-funcs.el --- Keybinding maps using general.el -*- lexical-binding: t; -*-

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

(use-package which-key
  :after evil
  :init (which-key-mode)
  :config
  (which-key-setup-minibuffer))

(pspmacs/load-inherit)
(provide 'pspmacs-keybindings)
;;; pspmacs-keybindings.el ends here

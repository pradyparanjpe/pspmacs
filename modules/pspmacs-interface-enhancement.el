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

  (general-create-definer pspmacs/leader-keys
    :states '(normal insert visual emacs)
    :keymaps 'override
    :prefix "SPC"
    :global-prefix "M-SPC")

  (general-create-definer pspmacs/local-leader-keys
    :states '(normal insert visual emacs)
    :keymaps 'override
    :prefix ","
    :global-prefix "M-,")

  (general-nmap ",," (general-simulate-key "C-c C-c" :which-key "C-c C-c"))
  (general-nmap ",." (general-simulate-key "C-c C-k" :which-key "C-c C-k"))

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

  ;; (general-define-key "ESC" #'keyboard-escape-quit)
  (general-define-key
   :keymaps '(minibuffer-mode-map
              minibuffer-local-map
              read-extended-command-mode-map)
   "ESC" #'keyboard-escape-quit)

  ;; Toggle buffers
  (pspmacs/leader-keys
    "TAB" '((lambda ()
              (interactive)
              (switch-to-buffer (other-buffer (current-buffer) 1)))
            :wk "last buff"))

  ;; Clear
  (pspmacs/leader-keys
    "-" '(:ignore t :wk "Clear"))

  ;; Updates
  (pspmacs/leader-keys
    :states 'normal
    "+" '(:ignore t :wk "Update")
    "+e" '(:ignore t :wk "Emacs (Build)")
    "+em" '(emacs-repo :wk "Transient menu")
    "+e+" '(emacs-repo/repo-install :wk "Upgrade with defaults")
    "++" '(pspmacs/git-rebase :wk "PSPMacs Config"))

  ;; debug
  (pspmacs/leader-keys
    "!" '(:ignore t :wk "debug"))

  ;;smart wrap
  (pspmacs/leader-keys
    "(" '(:ignore t :wk "[ ]"))

  ;; mail using smtpmail and mu4e
  (pspmacs/leader-keys
    "<" '(:ignore t :wk "mail"))

  ;; Hard Indentation
  (pspmacs/leader-keys
    ">" '(:ignore t :wk "———→")
    "> TAB" '(indent-rigidly-right-to-tab-stop :wk "→→→")
    "> <backtab>" '(indent-rigidly-left-to-tab-stop :wk "←←←")
    "> >" '(indent-rigidly :wk "manually"))

  ;; terminal
  (pspmacs/leader-keys
    "'" '(:ignore t :wk "shell"))

  ;; Eval expressions, lisp symbols
  (pspmacs/leader-keys
    ":" '(:ignore t :wk "eval")
    ":d" 'eval-defun
    ":e" 'eval-expression
    ":l" 'eval-last-expression
    ":r" 'eval-region)

  ;; Emoji Menu (New in Emacs-29)
  (pspmacs/leader-keys "@" (general-simulate-key "C-x 8 e"
                             :which-key "Emoji"))

  ;; UTF-8
  (pspmacs/leader-keys
    "8" '(insert-char :wk "UTF-8"))

  ;; AI
  (pspmacs/leader-keys
    :keymaps 'org-mode-map
    "A"   '(:ignore t :wk "AI"))

  ;; bookmark
  (pspmacs/leader-keys
    "B" '(:ignore t :wk "Bookmark")
    "Bd" '(bookmark-delete :wk "del")
    "Bj" '(bookmark-jump :wk "jump")
    "Bl" '(list-bookmarks :wk "list"))

  ;; powerthesaurus
  (pspmacs/leader-keys
    "D" '(:ignore t :wk "Diction"))

  (pspmacs/leader-keys
    :keymaps 'org-mode-map
    "M"  '(:ignore t :wk "Mode")
    "M<" '(:ignore t :wk "mail")
    "Mc" '(:ignore t :wk "config")
    "Mm" '(:ignore t :wk "markup")
    "Mp" '(:ignore t :wk "prog")
    "Mw" '(:ignore t :wk "web"))

  ;; unix password store
  (pspmacs/leader-keys "P" '(pass :wk "Password"))

  ;; FlySpell
  (pspmacs/leader-keys
    "S" '(:ignore t :wk "flySpell"))

  ;; pspmacs
  (pspmacs/leader-keys
    "[" '(:ignote t :wk "PSPMacs")
    "[c" '((lambda ()
             (interactive)
             (find-file custom-file))
            :wk "custom file")
    "[w" '(:ignote t :wk "worktree")
    "[wl" '((lambda ()
              (interactive)
              (find-file local-emacs-dir))
            :wk "local")
    "[wp" '((lambda ()
              (interactive)
              (if pvt-emacs-dir
                  (find-file pvt-emacs-dir)
                (message "private work-tree is not declared.")))
            :wk "private")
    "[wg" '((lambda ()
              (interactive)
              (message "disabled"))
            :wk "global <disabled>"))

  ;; see pspmacs-notes.org
  (pspmacs/leader-keys
    :keymaps 'org-mode-map
    "a"   '(:ignore t :wk "agenda"))

  ;; buffer
  ;; see 'bufler' and 'popper'
  (pspmacs/leader-keys
    "b" '(:ignore t :wk "buffer")
    "b TAB" '((lambda ()
                (interactive)
                (switch-to-buffer (other-buffer (current-buffer) 1)))
              :wk "🔀")
    "b-" '(pspmacs/switch-to-minibuffer :wk "▭")
    "bd" '(kill-this-buffer :wk "😵")
    "bm" '((lambda () (interactive)
             (switch-to-buffer (get-buffer-create messages-buffer-name)))
           :wk "💬")
    "bn" '(next-buffer :wk "→")
    "bp" '(previous-buffer :wk "←")

    "br" '(revert-buffer :wk "🔁")

    "bw" '(read-only-mode :wk "👁🖉")
    "b C-d" '(pspmacs/kill-other-buffers :wk "del rest")

    ;; scratch
    "bs"  '(:ignore t :wk "🗒")
    "bs<" '(:ignore t :wk "mail")
    "bsc" '(:ignore t :wk "config")
    "bsm" '(:ignore t :wk "markup")
    "bsp" '(:ignore t :wk "prog")
    "bsw" '(:ignore t :wk "web"))

  ;; Plain modes
  (pspmacs/leader-keys
    "bss" '((lambda () (interactive)
              (customize-set-variable 'comment-start "→")
              (pspmacs/mode-scratch 'text-mode))
            :wk "plain text")

    "bs-" '((lambda () (interactive)
              (customize-set-variable 'comment-start "→")
              (pspmacs/mode-scratch 'fundamental-mode))
            :wk "fundamental"))

  ;; Comments
  (pspmacs/leader-keys
    "c" '(:ignore t :wk "comment")
    "cl" '(comment-line :wk "comment line")
    "cp" '(comment-region :wk "comment paragraph"))

  ;; describe
  (pspmacs/leader-keys
    "d" '(:ignore t :wk "describe"))

  ;; errors
  (pspmacs/leader-keys
    "e" '(:ignore t :wk "error"))

  ;; file operations
  (pspmacs/leader-keys
    "f" '(:ignore t :wk "file")
    "ff" '(find-file :wk "find")
    "fD" '((lambda ()
             (interactive)
             (delete-file (buffer-file-name)))
           :wk "delete")
    "fR" '(rename-file :wk "rename")
    "fs" '(save-buffer :wk "save file")
    "fS" '(write-file :wk "save as")
    "fy" '(pspmacs/yank-file-name :wk "Copy file name"))

  ;; git
  (pspmacs/leader-keys "g" '(:ignore t :wk "git"))

  ;; help
  (pspmacs/leader-keys "h" (general-simulate-key "C-h" :which-key "Help"))

  ;; internet
  (pspmacs/leader-keys "i" '(:ignore t :wk "inet"))

  ;; jumps
  (pspmacs/leader-keys
    "j" '(:ignore t :wk "jump"))

  ;; Language server protocol
  (pspmacs/leader-keys
    :states 'normal
    :keymaps 'prog-mode-map
    "l" '(:ignore t :wk "lang"))

  ;; marks
  (pspmacs/leader-keys
    "m" '(:ignore t :wk "mark")
    "mm" '(set-mark-command :wk "set")
    "mM" '((lambda () (interactive)
             (call-interactively 'set-mark-command)
             (call-interactively 'set-mark-command))
           :wk "set deactivated mark")
    "mx" '(exchange-point-and-mark :wk "x-change"))

  ;; org mode
  (pspmacs/leader-keys
    "o" '(:ignore t :wk "org"))

  ;; TODO:
  ;; set 'citar' and 'org-roam'

  ;; quit
  (pspmacs/leader-keys
    "q" '(:ignore t :wk "quit")
    "qQ" '(kill-emacs :wk "daemon")
    "qq" '(delete-frame :wk "client")
    "qr" '(restart-emacs :wk "and restart")
    "q C-f" '(delete-other-frames :wk "other frames"))

  ;; Registers *consult*
  (pspmacs/leader-keys
    "r" '(:ignore t :wk "register"))

  ;; Toggles
  (pspmacs/leader-keys
    "t"     '(:ignore t :wk "toggle")
    "t RET" '(pspmacs/toggle-var :wk "toggle arbitrary")
    "t#"    '(:ignore t :wk "line number")
    "t#d"   '((lambda () (interactive) (setq display-line-numbers nil))
              :wk "disable")
    "t#e"   '((lambda () (interactive) (setq display-line-numbers t))
              :wk "enable")
    "t#r"   '((lambda () (interactive) (setq display-line-numbers 'relative))
              :wk "relative")
    "t#v"   '((lambda () (interactive) (setq display-line-numbers 'visual))
              :wk "visual")
    "tm"    '(:ignore t :wk "major mode")
    "tmc"   '(:ignore t :wk "config")
    "tmm"   '(:ignore t :wk "markup")
    "tmp"   '(:ignore t :wk "prog")
    "tmw"   '(:ignore t :wk "web")

    "tv"  '(visual-line-mode :wk "visual line"))

  ;; universal argument
  (pspmacs/leader-keys
    "u" '(universal-argument :wk "universal"))

  ;; Window
  (pspmacs/leader-keys
    "w" '(:ignore t :which-key "window")
    "w TAB" '(other-window :wk "◎ that")
    "w=" '(balance-windows-area :wk "balance")
    "wD" '(kill-buffer-and-window :wk "& buf: del")
    "w C-d" '(delete-other-windows :wk "del rest"))

  ;; Scratch buffers and mode-toggles
  (let* ((mode-toggle-binding nil)
         (scratch-binding nil))
    (dolist (maj-mode pspmacs/mode-keybindings nil)
      (let* ((key-seq (cdr maj-mode))
             (target-mode (car maj-mode))
             (wk-hint (string-replace
                       "-mode" ""
                       (symbol-name (car maj-mode)))))
        (push `(quote (,target-mode :wk ,wk-hint))
               mode-toggle-binding)
        (push (format "M%s" key-seq)
              mode-toggle-binding)
        (push `(quote ((lambda () (interactive)
                         (pspmacs/mode-scratch ',target-mode))
                       :wk ,wk-hint))
               scratch-binding)
        (push (format "bs%s" key-seq)
              scratch-binding)))
    (eval `(pspmacs/leader-keys ,@mode-toggle-binding))
    (eval `(pspmacs/leader-keys ,@scratch-binding))))

(use-package hydra
  :demand t)

(use-package pspmacs/startpage
  :ensure nil
  :commands pspmacs/startpage-set-up
  :config
  :general
  (pspmacs/leader-keys
    "bh" '(pspmacs/startpage-show :which-key "🏠")))
(pspmacs/startpage-set-up)

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
    "jj" '(avy-goto-char-timer :wk "search")))

(use-package evil
  :general
  ;; window navigations
  (pspmacs/leader-keys
    "wd" '(evil-window-delete :wk "😵")
    "wH" '(evil-window-move-far-left :wk "←←←")
    "wh" '(evil-window-left :wk "◎ ←")
    "wJ" '(evil-window-move-very-bottom :wk "↓↓↓")
    "wj" '(evil-window-down :wk "◎ ↓")
    "wK" '(evil-window-move-very-top :wk "↑↑↑")
    "wk" '(evil-window-up :wk "◎ ↑")
    "wL" '(evil-window-move-far-right :wk "→→→")
    "wl" '(evil-window-right :wk "◎ →")
    "wn" '(evil-window-next :wk "◎ next")
    "wp" '(evil-window-prev :wk "◎ prev")
    "ws" '(evil-window-split :wk "-split-")
    "wv" '(evil-window-vsplit :wk "spl|it"))
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
   ;; fixes weird tab behaviour
   evil-want-C-i-jump nil)
  :custom
  (evil-search-module 'isearch)
  (evil-split-window-below t)
  (evil-vsplit-window-right t)
  (evil-undo-system 'undo-tree)

  :config
  (setq evil-normal-state-cursor '(box "orange"))
  (setq evil-insert-state-cursor '((bar . 3) "green"))
  (setq evil-visual-state-cursor '(box "light blue"))
  (setq evil-replace-state-cursor '(box "yellow"))
  (evil-mode t) ;; globally enable evil mode
  ;; default mode: normal
  (evil-set-initial-state 'messages-buffer-mode 'normal)
  ;; default mode: insert
  (evil-set-initial-state 'eshell-mode 'insert)
  (evil-set-initial-state 'magit-diff-mode 'insert))

(use-package evil-collection ;; evilifies a bunch of things
  :after evil
  :demand t
  :custom
  ;; (evil-collection-outline-bind-tab-p t)
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
    "SPC" '(execute-extended-command :wk "M-x"))

  :init
  (vertico-mode)
  :hook
  (('rfn-eshadow-update-overlay . vertico-directory-tidy)))

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
   "C-<escape>" #'marginalia-cycle)
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
  :custom
  (tempel-trigger-prefix "<")

  :general
  (pspmacs/leader-keys
    ">t" '(:ignore t :wk "tempel templates")
    ">t>" '(tempel-complete :wk "complete")
    ">ti" '(tempel-insert :wk "insert"))

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
(use-package tempel-collection
  :after tempel)

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

(use-package pspmacs/pspline
  :ensure nil
  :commands pspmacs/pspline-set-up
  :config
  (battery))
(pspmacs/pspline-set-up)

(use-package solaire-mode
  :config
  (solaire-global-mode +1))

(defhydra hydra-zoom (global-map "<f8>")
  "zoom"
  ("g" text-scale-increase "in")
  ("l" text-scale-decrease "out"))

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

  ;;; Font
  (if (daemonp)
      (add-hook 'after-make-frame-functions
                (lambda (frame)
                  (with-selected-frame frame
                    (pspmacs/set-font-faces))))
    (pspmacs/set-font-faces))

  (global-set-key (kbd "C-=") 'text-scale-increase)
  (global-set-key (kbd "C--") 'text-scale-decrease)

  :custom
  ;; Vertico suggestions
  ;; Emacs 28: Hide commands in M-x which do not work in the current mode.
  ;; Vertico commands are hidden in normal buffers.
  (read-extended-command-predicate #'command-completion-default-include-p)
  (scroll-margin 5)
  (indent-tabs-mode nil)
  (tab-width 4)
  (svg-lib-icons-dir (expand-file-name "svg-lib" xdg/emacs-cache-directory))
  (use-dialog-box nil)
  ;; Vertico suggestions
  ;; Do not allow the cursor in the minibuffer prompt
  (minibuffer-prompt-properties
   '(read-only t cursor-intangible t face minibuffer-prompt))
  ;; Vertico suggestions
  ;; Enable recursive minibuffers
  (enable-recursive-minibuffers t)
  (abbrev-file-name (expand-file-name "abbrev_defs" xdg/emacs-state-directory))
  :hook
  (minibuffer-setup . cursor-intangible-mode)

  :config
  (advice-add #'completing-read-multiple :filter-args #'crm-indicator)
  ;;; locale
  (set-charset-priority 'unicode))

(pspmacs/load-inherit)
;;; pspmacs-interface-enhancement.el ends here

;;; pspmacs-interface-enhancement.el --- User experience/interface -*- lexical-binding: t; -*-

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

;;; Garbage collector
(use-package gcmh
  :demand t
  :config
  (gcmh-mode 1))

(use-package evil
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
  ;; following need to be \\='set\\=', cannot be customized
  (setq
   evil-normal-state-cursor '(box "#ff9f00")
   evil-insert-state-cursor '((bar . 3) "#00cf6f")
   evil-visual-state-cursor '(box "#009fff")
   evil-replace-state-cursor '(box "#ffff00")
   evil-operator-state-cursor '(box "#ff009f")
   evil-motion-state-cursor '(box "#3fffff")
   evil-emacs-state-cursor '(box "#bfbfbf"))

  (evil-mode t) ;; globally enable evil mode

  ;; default mode: normal
  (evil-set-initial-state 'messages-buffer-mode 'normal)

  ;; default mode: insert
  (evil-set-initial-state 'eshell-mode 'insert)
  (evil-set-initial-state 'magit-diff-mode 'insert))

(use-package general
  :after evil
  :demand t
  :config
  (general-evil-setup)
  ;; integrate general with evil

  ;; Mask some evil bindings
  (general-define-key :keymaps 'evil-motion-state-map "RET" nil)
  (general-define-key :keymaps 'evil-insert-state-map "C-k" nil)

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

  ;; Clear
  (pspmacs/leader-keys
    "-" '(:ignore t :wk ":Clear"))

  ;; Updates
  (pspmacs/leader-keys
    :states 'normal
    "+"   '(:ignore t :wk ":Up")
    "+e"  '(:ignore t :wk ":Emacs (Build)")
    "+em" '(build-emacs :wk ":Transient menu")
    "+e+" '(build-emacs/repo-install :wk ":Upgrade with defaults")
    "++"  '(pspmacs/git-rebase :wk ":PSPMacs Config"))

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
    ":" '(:ignore t :wk ":Eval")
    ":d" '(eval-defun :wk "efun")
    ":e" '(eval-expression :wk "xp")
    ":l" '(eval-last-expression :wk "ast-exp")
    ":r" '(eval-region :wk "egion"))

  ;; Emoji Menu (New in Emacs-29)
  (pspmacs/leader-keys "@" (general-simulate-key "C-x 8 e" :which-key "😀"))

  ;; Treemacs
  (pspmacs/leader-keys
    "0" '(:ignore t :wk ":🌳"))

  ;; UTF-8
  (pspmacs/leader-keys
    "8" '(insert-char :wk "-UTF"))

  ;; AI
  (pspmacs/leader-keys
    "A" '(:ignore t :wk "I"))

  ;; bookmark
  (pspmacs/leader-keys
    "B" '(:ignore t :wk "ukmrk")
    "Bd" '(bookmark-delete :wk "del")
    "Bj" '(bookmark-jump :wk "ump")
    "Bl" '(list-bookmarks :wk "ist"))

  ;; powerthesaurus
  (pspmacs/leader-keys
    "D" '(:ignore t :wk "ict"))

  ;; font
  (pspmacs/leader-keys
    "F" '(:ignore t :wk "ont"))

  (pspmacs/leader-keys
    :keymaps 'org-mode-map
    "M"  '(:ignore t :wk "ode")
    "M<" '(:ignore t :wk "mail")
    "Mc" '(:ignore t :wk "onfig")
    "Mm" '(:ignore t :wk "arkup")
    "Mp" '(:ignore t :wk "rog")
    "Mw" '(:ignore t :wk "eb"))

  ;; unix password store
  (pspmacs/leader-keys "P" '(pass :wk "asswd"))

  ;; FlySpell
  (pspmacs/leader-keys
    "S" '(:ignore t :wk "pell"))

  ;; pspmacs
  (pspmacs/leader-keys
    "[" '(:ignote t :wk "PSP]")
    "[c" '((lambda () (interactive) (find-file custom-file)) :wk "ustom")
    "[w" '(:ignote t :wk "orktree")
    "[wl" '((lambda () (interactive) (find-file local-emacs-dir)) :wk "ocal")
    "[wp" '((lambda ()
              (interactive)
              (if pvt-emacs-dir
                  (find-file pvt-emacs-dir)
                (message "private work-tree is not declared.")))
            :wk "rivate")
    "[wg" '((lambda ()
              (interactive)
              (message "disabled"))
            :wk "lobal <disabled>"))

  ;; input method
  (pspmacs/leader-keys
    "\\" '(:ignore t :wk ":Kbd")
    "\\\\" '(toggle-input-method :wk ":Toggle")
    "\\\|" '(set-input-method :wk ":Interactive"))

  ;; see pspmacs-notes.org
  (pspmacs/leader-keys
    :keymaps 'org-mode-map
    "a"   '(:ignore t :wk "genda"))

  ;; buffer
  ;; see 'bufler'
  (pspmacs/leader-keys
    "b" '(:ignore t :wk "uf")
    "b TAB" '((lambda ()
                (interactive)
                (switch-to-buffer (other-buffer (current-buffer) 1)))
              :wk "🔀")
    "b-" '(pspmacs/switch-to-minibuffer :wk "▭")
    "bd" '(kill-current-buffer :wk "😵")
    "bm" '((lambda () (interactive)
             (switch-to-buffer (get-buffer-create messages-buffer-name)))
           :wk "💬")
    "bn" '(next-buffer :wk "→")
    "bp" '(previous-buffer :wk "←")

    "br" '(revert-buffer :wk "🔁")

    "bw" '(read-only-mode :wk "👁🖉")
    "b C-d" '(pspmacs/kill-other-buffers :wk "😵 rest")

    ;; scratch
    "bs"  '(:ignore t :wk "🗒")
    "bs<" '(:ignore t :wk ":Mail")
    "bsc" '(:ignore t :wk "onfig")
    "bsm" '(:ignore t :wk "arkup")
    "bsp" '(:ignore t :wk "rog")
    "bsw" '(:ignore t :wk "eb"))

  ;; Plain modes
  (pspmacs/leader-keys
    "bss" '((lambda () (interactive)
              (customize-set-variable 'comment-start "→")
              (pspmacs/mode-scratch 'text-mode))
            :wk ":Plain text")

    "bs-" '((lambda () (interactive)
              (customize-set-variable 'comment-start "→")
              (pspmacs/mode-scratch 'fundamental-mode))
            :wk ":Fundamental"))

  ;; Comments
  (pspmacs/leader-keys
    "c" '(:ignore t :wk "omnt")
    "cl" '(comment-line :wk "ine")
    "cp" '(comment-region :wk "aragraph"))

  ;; describe
  (pspmacs/leader-keys
    "d" '(:ignore t :wk "esc"))

  ;; errors
  (pspmacs/leader-keys
    "e" '(:ignore t :wk "rr"))

  ;; file operations
  (pspmacs/leader-keys
    "f" '(:ignore t :wk "ile")
    "ff" '(find-file :wk "ind")
    "fD" '((lambda ()
             (interactive)
             (delete-file (buffer-file-name)))
           :wk "elete")
    "fR" '(rename-file :wk "ename")
    "fs" '(save-buffer :wk "ave")
    "fS" '(write-file :wk "ave as")
    "fy" '(pspmacs/yank-file-name :wk "ank name"))

  ;; git
  (pspmacs/leader-keys "g" '(:ignore t :wk "it"))

  ;; help
  (pspmacs/leader-keys "h" (general-simulate-key "C-h" :which-key ":C-h"))

  ;; internet
  (pspmacs/leader-keys "i" '(:ignore t :wk "net"))

  ;; jumps
  (pspmacs/leader-keys
    "j" '(:ignore t :wk "ump"))

  ;; Language server protocol
  (pspmacs/leader-keys
    :states 'normal
    :keymaps 'prog-mode-map
    "l" '(:ignore t :wk "ang"))

  ;; marks
  (pspmacs/leader-keys
    "m" '(:ignore t :wk "ark")
    "mm" '(set-mark-command :wk ":Set")
    "mM" '((lambda () (interactive)
             (call-interactively 'set-mark-command))
           :wk "ark visual")
    "mx" '(exchange-point-and-mark :wk ":Exchange"))

  ;; org mode
  (pspmacs/leader-keys
    "o" '(:ignore t :wk "rg"))

  ;; TODO:
  ;; set 'citar' and 'org-roam'

  (pspmacs/leader-keys
    "p" '(:ignore t :wk "roj"))

  ;; quit
  (pspmacs/leader-keys
    "q" '(:ignore t :wk "uit")
    "qQ" '(kill-emacs :wk ":Daemon")
    "qq" '(delete-frame :wk ":Client")
    "qr" '(restart-emacs :wk "estart")
    "q C-d" '(delete-other-frames :wk ":Other frames"))

  ;; Registers *consult*
  (pspmacs/leader-keys
    "r" '(:ignore t :wk "eg"))

  ;; Toggles
  (pspmacs/leader-keys
    "t"     '(:ignore t :wk "ogl")
    "t RET" '(pspmacs/toggle-var :wk "variable")
    "t#"    '(:ignore t :wk ":Number")
    "t#d"   '((lambda () (interactive) (setq display-line-numbers nil))
              :wk "isable")
    "t#e"   '((lambda () (interactive) (setq display-line-numbers t))
              :wk "nable")
    "t#r"   '((lambda () (interactive) (setq display-line-numbers 'relative))
              :wk "elative")
    "t#v"   '((lambda () (interactive) (setq display-line-numbers 'visual))
              :wk "isual")
    "tm"    '(:ignore t :wk "ajor mode")
    "tmc"   '(:ignore t :wk "onfig")
    "tmm"   '(:ignore t :wk "arkup")
    "tmp"   '(:ignore t :wk "rog")
    "tmw"   '(:ignore t :wk "eb")

    "tv"  '(visual-line-mode :wk "isual line"))

  ;; universal argument
  (pspmacs/leader-keys "u" '(universal-argument :wk ":C-u"))

  ;; Window
  (pspmacs/leader-keys
    "w" '(:ignore t :wk "in")
    "w TAB" '(other-window :wk ":Back ◎")
    "w=" '(balance-windows-area :wk ":Balance")
    "wD" '(kill-buffer-and-window :wk "el & buf")
    "w C-d" '(delete-other-windows :wk "😵 rest")
    "wd" '(evil-window-delete :wk "😵")
    "wH" '(evil-window-move-far-left :wk "←←←")
    "wh" '(evil-window-left :wk "← ◎")
    "wJ" '(evil-window-move-very-bottom :wk "↓↓↓")
    "wj" '(evil-window-down :wk "↓ ◎")
    "wK" '(evil-window-move-very-top :wk "↑↑↑")
    "wk" '(evil-window-up :wk "↑ ◎")
    "wL" '(evil-window-move-far-right :wk "→→→")
    "wl" '(evil-window-right :wk "→ ◎")
    "wn" '(evil-window-next :wk "ext ◎")
    "wp" '(evil-window-prev :wk "rev ◎")
    "ws" '(evil-window-split :wk "-split-")
    "wv" '(evil-window-vsplit :wk "spl|it"))

  ;; Scratch buffers and mode-toggles
  (let* ((mode-toggle-binding nil)
         (scratch-binding nil))
    (dolist (maj-mode pspmacs/mode-keybindings nil)
      (let* ((key-seq (cdr maj-mode))
             (target-mode (car maj-mode))
             (wk--hint
              (string-replace "-mode" "" (symbol-name (car maj-mode))))
             (wk-hint
              (if (string= (substring wk--hint 0 1) (substring key-seq -1))
                  (substring wk--hint 1) (format":%s" wk--hint))))
        (push `(quote (,target-mode :wk ,wk-hint)) mode-toggle-binding)
        (push (format "M%s" key-seq) mode-toggle-binding)
        (push `(quote ((lambda () (interactive)
                         (pspmacs/mode-scratch ',target-mode))
                       :wk ,wk-hint))
              scratch-binding)
        (push (format "bs%s" key-seq) scratch-binding)))
    (eval `(pspmacs/leader-keys ,@mode-toggle-binding))
    (eval `(pspmacs/leader-keys ,@scratch-binding))))

(use-package hydra
  :demand t)

(use-package pspmacs/startpage
  :ensure nil
  :commands pspmacs/startpage-set-up
  :general
  (pspmacs/leader-keys
    "bh" '(pspmacs/startpage-show :wk "🏠")))

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
    "jj" '(avy-goto-char-timer :wk ":Search")))

(use-package ace-window
  :demand t
  :general
  (pspmacs/leader-keys
    "wf" '(ace-window :wk ":Hint"))
  :config
  (ace-window-display-mode -1))

(use-package popper
  :general
  (general-define-key
   "C-<tab>" '(popper-toggle :wk "toggle")
   "C-<iso-lefttab>" '(popper-toggle :wk "cycle"))

  (pspmacs/leader-keys
    "wt" '(popper-toggle-type :wk "ype pop")
    "wx" '(popper-kill-latest-popup :wk ":Kill pop"))

  :init
  (popper-mode 1)
  (popper-echo-mode 1)

  :custom
  ;; (popper-group-function #'popper-group-by-project)  ; configure project.el
  (popper-display-control t)  ; replace this with shackle.el if necessary
  (popper-mode-line "🫣")
  (popper-reference-buffers
   '("\\*Messages\\*"
     "Output\\*$"
     "\\*Async Shell Command\\*"
     "\\*Edit Annotation .*\\.pdf\\*$"

     help-mode
     helpful-mode
     compilation-mode

     ;; shells
     ;; eshell as a popup
     "^\\*eshell.*\\*$"
     eshell-mode

     ;; shell as a popup
     "^\\*shell.*\\*$"
     shell-mode

     ;; term as a popup
     "^\\*term.*\\*$"
     term-mode

     ;; vterm as a popup
     "^\\*vterm.*\\*$"
     vterm-mode

     ;; eat as a popup
     "^\\*eat.*\\*$"
     eat-mode)))

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
    "SPC" '(execute-extended-command :wk ":M-x"))

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
    ">t" '(:ignore t :wk "emplates")
    ">t>" '(tempel-complete :wk ":Complete")
    ">ti" '(tempel-insert :wk "nsert"))

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
  :after modus-themes
  :ensure nil
  :demand t
  :commands pspmacs/pspline-set-up
  :custom-face
  (pspmacs/pspline-buffer-process
   ((default
     (:foreground ,(modus-themes-get-color-value 'modeline-info) :box t))))
  :config
  (battery)
  (pspmacs/pspline-set-up))

(use-package solaire-mode
  :config
  (solaire-global-mode +1))

(use-package hyperbole
  :after ace-window
  :demand t
  :general
  (pspmacs/leader-keys :keymaps 'hyperbole-mode-map
    "RET" '(hkey-either :wk ":Hyper")
    "wg" '(hycontrol-windows-grid :wk "rid")
    "wm" '(hycontrol-make-windows-grid :wk "ake grid"))
  :custom
  (hsys-org-enable-smart-keys t)
  (hbmap:dir-user (xdg/make-path "hyperb"))
  (hbmap:dir-filename (xdg/make-path "hyperb/HBMAP"))
  (hyrolo-file-list `(,(xdg/make-path "rolo.otl")))
  :config
  (hyperbole-mode 1)
  (hkey-ace-window-setup)
  ;; `hkey-ace-window-setup' turns `ace-window-display-mode' back on.
  (ace-window-display-mode -1))

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
  :custom
  ;; Vertico suggestions
  ;; Emacs 28: Hide commands in M-x which do not work in the current mode.
  ;; Vertico commands are hidden in normal buffers.
  (read-extended-command-predicate #'command-completion-default-include-p)
  (scroll-margin 5)
  (indent-tabs-mode nil)
  (tab-width 4)
  (svg-lib-icons-dir (xdg/make-path "svg-lib" 'cache))
  (use-dialog-box nil)
  ;; Vertico suggestions
  ;; Do not allow the cursor in the minibuffer prompt
  (minibuffer-prompt-properties
   '(read-only t cursor-intangible t face minibuffer-prompt))
  ;; Vertico suggestions
  ;; Enable recursive minibuffers
  (enable-recursive-minibuffers t)
  (abbrev-file-name (xdg/make-path "abbrev_defs" 'state))
  :hook
  (minibuffer-setup . cursor-intangible-mode)

  :config
  (advice-add #'completing-read-multiple :filter-args #'crm-indicator)
  ;;; locale
  (set-charset-priority 'unicode))

(pspmacs/load-inherit)
;;; pspmacs-interface-enhancement.el ends here

;;; programming.el --- common programming config -*- lexical-binding: t; -*-

;; Copyright (C) 2023  Pradyumna Swanand Paranjape

;; Author: Pradyumna Swanand Paranjape <pradyparanjpe@rediffmail.com>
;; Keywords: help, languages

;; This programmingram is free software; you can redistribute it and/or modify
;; it under the terms of the GNU Lesser General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This programmingram is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU Lesser General Public License for more details.

;; You should have received a copy of the GNU Lesser General Public License
;; along with this programmingram.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;;; Code:

(use-package corfu
  ;; Optional customizations
  :custom
  (corfu-cycle t)                ;; Enable cycling for `corfu-next/previous'
  (corfu-auto t)                 ;; Enable auto completion
  ;; (corfu-separator ?\s)          ;; Orderless field separator
  ;; (corfu-quit-at-boundary nil)   ;; Never quit at completion boundary
  ;; (corfu-quit-no-match nil)      ;; Never quit, even if there is no match
  ;; (corfu-preview-current nil)    ;; Disable current candidate preview
  (corfu-preselect 'prompt)      ;; Preselect the prompt
  ;; (corfu-on-exact-match nil)     ;; Configure handling of exact matches
  ;; (corfu-scroll-margin 5)        ;; Use scroll margin

  ;; Enable Corfu only for certain modes.
  ;; :hook ((prog-mode . corfu-mode)
            ;;        (shell-mode . corfu-mode)
            ;;        (eshell-mode . corfu-mode)
            ;;        (org-mode . corfu-mode))
  ;; ;; Recommended: Enable Corfu globally.
  ;; ;; This is recommended since Dabbrev can be used globally (M-/).
  ;; See also `corfu-excluded-modes'.
  :config
  (global-corfu-mode))

  ;; A few more useful configurations...
(use-package emacs
  :init
  ;; TAB cycle if there are only few candidates
  (setq completion-cycle-threshold 3)
  ;; Emacs 28: Hide commands in M-x which do not apply to the current mode.
  ;; Corfu commands are hidden, since they are not supposed to be used via M-x.
  (setq read-extended-command-predicate
        #'command-completion-default-include-p)

  ;; Enable indentation+completion using the TAB key.
  ;; `completion-at-point' is often bound to M-TAB.
  (setq tab-always-indent 'complete))

(use-package cape
  :init
  ;; Add `completion-at-point-functions', used by `completion-at-point'.
  (add-to-list 'completion-at-point-functions #'cape-dabbrev t)
  (add-to-list 'completion-at-point-functions #'cape-file t))
  ;; (fset #'cape-path (cape-company-to-capf #'company-files))
  ;; (add-to-list 'completion-at-point-functions #'cape-path t)
  ;;(add-to-list 'completion-at-point-functions #'cape-history)
  ;;(add-to-list 'completion-at-point-functions #'cape-keyword)
  ;;(add-to-list 'completion-at-point-functions #'cape-tex)
  ;;(add-to-list 'completion-at-point-functions #'cape-sgml)
  ;;(add-to-list 'completion-at-point-functions #'cape-rfc1345)
  ;;(add-to-list 'completion-at-point-functions #'cape-abbrev)
  ;;(add-to-list 'completion-at-point-functions #'cape-ispell)
  ;;(add-to-list 'completion-at-point-functions #'cape-dict)
  ;;(add-to-list 'completion-at-point-functions #'cape-symbol)
  ;;(add-to-list 'completion-at-point-functions #'cape-line)

(use-package kind-icon
  :ensure t
  :after corfu
  :custom
  (kind-icon-default-face 'corfu-default) ; to compute blended backgrounds correctly
    :config
    (add-to-list 'corfu-margin-formatters #'kind-icon-margin-formatter))

(use-package corfu-terminal
  :straight (:type git :repo "https://codeberg.org/akib/emacs-corfu-terminal.git")
  :config
  (unless (display-graphic-p)
    (corfu-terminal-mode t)))

(use-package corfu-doc-terminal
  :straight (:type git :repo "https://codeberg.org/akib/emacs-corfu-doc-terminal.git")
  :config
  (unless (display-graphic-p)
    (corfu-doc-terminal-mode t)))

(use-package gtags
  :hook (prog-mode . gtags-mode))

(use-package lsp-mode
  :commands (lsp lsp-deferred)
  :general
  (pspmacs/local-leader-keys
    :states 'normal
    :keymaps
    'lsp-mode-map
    "i" '(:ignore t :which-key "import")
    "i o" '(lsp-organize-imports :wk "optimize")
    "l" '(:keymap lsp-command-map :wk "lsp")
    "a" '(lsp-execute-code-action :wk "code action"))
  (pspmacs/leader-keys
    :states 'normal
    "e" '(:ignore t :wk "errors")
    "el" '(lsp-treemacs-errors-list :wk "list"))

  :custom
  (lsp-restart 'ignore)
  (lsp-session-file (expand-file-name
                     ".lsp-session-v1" xdg/emacs-state-directory))
  ;; (lsp-eldoc-enable-hover nil)
  ;; (lsp-enable-file-watchers nil)
  ;; (lsp-signature-auto-activate nil)
  ;; (lsp-modeline-diagnostics-enable nil)
  ;; (lsp-keep-workspace-alive nil)
  ;; (lsp-auto-execute-action nil)
  ;; (lsp-before-save-edits nil)
  ;; (lsp-headerline-breadcrumb-enable nil)
  ;; (lsp-diagnostics-provider :none)
  :hook (python-mode . lsp-deferred))

(use-package lsp-ui
  :general
  (lsp-ui-peek-mode-map
   :states 'normal
   "C-j" 'lsp-ui-peek--select-next
   "C-k" 'lsp-ui-peek--select-prev)

  (outline-mode-map
   :states 'normal
   "C-j" 'nil
   "C-k" 'nil)

  :init
  (defun pspmacs/lsp-ui-disable-modes ()
    "Disable certian modes from lsp-ui"
    (display-line-numbers-mode -1)
    (whitespace-mode -1))
  :custom
  ;; (lsp-ui-doc-show-with-mouse nil)
  (lsp-ui-doc-show-with-cursor t)
  (lsp-ui-peek-always-show t)
  (lsp-ui-peek-fontify 'always)

  :config
  (setq lsp-ui-doc-enable t
        lsp-ui-doc-delay 1)
  :hook (lsp-mode . lsp-ui-mode)
  (lsp-ui-doc-frame-mode . pspmacs/lsp-ui-disable-modes))

(use-package flycheck
  :general
  (pspmacs/leader-keys
    "en" '(flycheck-next-error :wk "next error")
    "ep" '(flycheck-previous-error :wk "previous error"))
  :custom
  (flycheck-indication-mode 'right-fringe) 
  (flycheck-check-syntax-automatically '(mode-enabled save))
  :hook
  ((lsp-mode . flycheck-mode)
   (envrc-mode . (lambda ()
           (setq flycheck-python-flake8-executable
             (executable-find "python"))
           (setq flycheck-checker 'python-flake8)
           (setq flycheck-flake8rc ".flake8")))))

(use-package eldoc
  :hook (emacs-lisp-mode cider-mode))

(use-package display-fill-column-indicator
  :demand t
  :hook
  (prog-mode . display-fill-column-indicator-mode)
  :init
  (setq-default fill-column 80))

(pspmacs/load-inherit)
;;; pspmacs-programming.el ends here
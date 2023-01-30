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

(use-package company
  :ensure t
  :diminish
  :commands (company-mode company-indent-or-complete-common)
  :custom
  (company-dabbrev-other-buffers t)
  (company-dabbrev-code-other-buffers t)
  :hook ((text-mode . company-mode)
         (prog-mode . company-mode)))

(use-package company-quickhelp
  :after company
  :general
  (pspmacs/leader-keys
    :keymaps 'company-active-map
    "H" '(:ignore t :wk "quickhelp")
    "H?" '(company-quickhelp-manual-begin)))

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
                     ".lsp-session-v1" xdg/emacs-cache-directory))
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

   :custom
   ;; (lsp-ui-doc-show-with-mouse nil)
   (lsp-ui-doc-show-with-cursor t)
   (lsp-ui-peek-always-show t)
   (lsp-ui-peek-fontify 'always)
   :config
   (setq lsp-ui-doc-enable t
     lsp-ui-doc-delay 1)
   :hook (lsp-mode . lsp-ui-mode))

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
  (programming-mode . display-fill-column-indicator-mode)
  :init
  (setq-default fill-column 80))

(pspmacs/load-inherit)

;;; pspmacs-programming.el ends here

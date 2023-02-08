;;; prog.el --- python ide -*- lexical-binding: t; -*-

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

(use-package python-mode
  :mode ("\\.py\\'" . python-mode)
  :interpreter ("python" . python-mode)
  :general
  (pspmacs/local-leader-keys
    :keymaps 'python-mode-map
    "'" '((lambda () (interactive)
            (pspmacs/inferior-interpreter 'run-python))
          :wk "python"))
  (python-mode-map :states 'normal "gz" nil "C-j" nil)
  ;; (python-mode-map :states 'insert "TAB" 'pspmacs/py-indent-or-complete)
  :init
  (with-eval-after-load 'lsp-mode
    (when
        (string= major-mode "python-mode")
      (pspmacs/extend-list
       'lsp-file-watch-ignored-directories
       '(
         ;; python directories
         "[/\\\\]docs\\'"
         "[/\\\\]build\\'"
         "[/\\\\]tests\\'"
         "[/\\\\]\\.?venv\\'"
         "[/\\\\]\\.?\\(\\([a-zA-Z0-9]\\)*_?\\)*\\.egg-info\\'"
         "[/\\\\]\\.?\\(\\([a-zA-Z0-9]\\)*_?\\)*cache\\(__\\)?\\'"))))
  :custom
  (python-indent-offset 0)
  (python-shell-interpreter-args "-i --simple-prompt --no-color-info")
  (python-shell-prompt-regexp "In \\[[0-9]+\\]: ")
  (python-shell-prompt-block-regexp "\\.\\.\\.\\.: ")
  (python-shell-prompt-output-regexp "Out\\[[0-9]+\\]: ")
  (python-shell-completion-setup-code
   "from IPython.core.completerlib import module_completion")
  (python-shell-completion-string-code
   "';'.join(get_ipython().Completer.all_completions('''%s'''))\n")
  :config
  (sp-local-pair 'python-mode "\"\"\"" "\"\"\"")
  (sp-local-pair 'python-mode "__" "__")

  :hook
  ((python-mode . (lambda ()
                   (when (executable-find "ipython")
                     (setq python-shell-interpreter
                           (executable-find "ipython")))))
   (python-mode . pspmacs/prettify-python)
   (python-mode . pspmacs/pyfaces)))

(use-package pyvenv-auto
  :defer t
  :hook ((python-mode . pyvenv-auto-run)))

(use-package importmagic
  :defer t
  :general
  (pspmacs/local-leader-keys
    :states 'normal
    :keymaps 'importmagic-mode-map
    "rf" '(importmagic-fix-symbol-at-point :wk "fix at point")
    "ri" '(importmagic-fix-imports :wk "fix imports")
    "rI" '(importmagic-fix-symbol :wk "arbitrary import"))
  :hook
  (python-mode . importmagic-mode))

(use-package isortify
  :defer t
  :hook (python-mode . isortify-mode))

(use-package yapfify
  :defer t
  :hook (python-mode . yapf-mode))

(use-package lsp-pyright
  :defer t
  :hook (python-mode . (lambda () (require 'lsp-pyright) (lsp-deferred))))

(use-package py-snippets
  :after '(yasnippet python-mode)
  :config
  (py-snippets-initialize))

(pspmacs/load-inherit)
;;; pspmacs-prog.el ends here

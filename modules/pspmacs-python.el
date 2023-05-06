;;; python.el --- python ide -*- lexical-binding: t; -*-

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

(use-package python-mode
  :mode ("\\.py\\'" . python-mode)
  :interpreter ("python" . python-mode)
  :general
  (pspmacs/local-leader-keys
    :keymaps 'python-mode-map
    "'" '((lambda () (interactive)
            (pspmacs/inferior-interpreter 'run-python))
          :wk "python"))
  (general-def 'normal
    python-mode-map
    "gz" nil
    "C-j" nil)
  :init
  (with-eval-after-load 'eglot
    (add-to-list 'eglot-server-programs
                 '(python-mode . ("pyright-langserver" "--stdio"))))
  :custom
  (python-indent-offset 0)
  :config
  (sp-local-pair 'python-mode "\"\"\"" "\"\"\"")
  (sp-local-pair 'python-mode "'''" "'''")
  (sp-local-pair 'python-mode "__" "__")
  :hook
  ((python-mode . pspmacs/prefer-interpreter-ipython)
   (python-mode . pspmacs/prettify-python)
   (python-mode . pspmacs/pyfaces)))

(defun pyrightconfig-write (virtualenv)
  "Taken from https://robbmann.io/posts/emacs-eglot-pyrightconfig/"
  (interactive "DEnv: ")
  (let* (;; file-truename and tramp-file-local-name ensure that neither `~'
         ;; nor the Tramp prefix (e.g. "/ssh:my-host:") wind up in the final
         ;; absolute directory path.
         (venv-dir (tramp-file-local-name (file-truename virtualenv)))

         ;; Given something like /path/to/.venv/,
         ;; this strips off the trailing `/'.
         (venv-file-name (directory-file-name venv-dir))

         ;; Naming convention for venvPath matches the field
         ;; for pyrightconfig.json. `file-name-directory' gets us
         ;; the parent path (one above .venv).
         (venvPath (file-name-directory venv-file-name))

         ;; Grabs just the `.venv' off the end of the venv-file-name.
         (venv (file-name-base venv-file-name))

         ;; Eglot demands that `pyrightconfig.json'
         ;; is in the project root folder.
         (base-dir (vc-git-root default-directory))
         (out-file (expand-file-name "pyrightconfig.json" base-dir))

         ;; Finally, get a string with the JSON payload.
         (out-contents (json-encode `(:venvPath ,venvPath :venv ,venv))))

    ;; Emacs uses buffers for everything.  This creates a temp buffer,
    ;; inserts the JSON payload, then flushes that content to final
    ;; `pyrightconfig.json' location
    (with-temp-file out-file (insert out-contents))))

(use-package ein
  :demand t
  :config
  (add-to-list 'org-babel-load-languages '(ein . t)))

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

(use-package py-snippets
  :after '(yasnippet python-mode)
  :config
  (py-snippets-initialize))

(pspmacs/load-inherit)
;;; pspmacs-python.el ends here

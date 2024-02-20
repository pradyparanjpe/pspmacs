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
  :after smartparens
  :mode ("\\.py\\'" . python-mode)
  :interpreter ("python" . python-mode)
  :general
  (pspmacs/local-leader-keys
    :keymaps 'python-mode-map
    "'" '((lambda () (interactive)
            (pspmacs/inferior-interpreter #'run-python))
          :wk "python"))
  (pspmacs/leader-keys
    "'p" '(run-python :wk ""))
  (general-def 'normal
    python-mode-map
    "gz" nil
    "C-j" nil)
  :init
  ;; (with-eval-after-load 'eglot
  ;; (add-to-list 'eglot-server-programs
  ;; '(python-mode . ("pyright-langserver" "--stdio"))))
  :custom
  (python-indent-offset 4)
  ;; Global python-lsp-server configuration
  (eglot-workspace-configuration
   `(:pylsp .
            (:plugins
             (:jedi_completion (:include_params t :fuzzy t)
                               :rope (:enabled t  :json-false)
                               :rope_autoimport (:enabled t)
                               :pyflakes (:enabled t :json-false)
                               :mccabe (:enabled t :json-false)
                               :pycodestyle (:enabled t :json-false)
                               :pydocstyle (:enabled t :convention "numpy")
                               :pylint (:enabled :json-false)
                               :pylsp_mypy (:enabled nil)
                               :yapf (:enabled t :json-false)
                               :flake8 (:enabled nil)))))
  (python-shell-dedicated t)
  :hook
  ((python-mode . pspmacs/prefer-interpreter-ipython)
   (python-mode . pspmacs/prettify-python)
   (python-mode . pspmacs/pyfaces))
  :config
  (require 'numpydoc))

(use-package jupyter
  :general
  (pspmacs/leader-keys
    "'i" '(jupyter-run-repl :wk "ipy"))
  :custom
  (org-babel-default-header-args:jupyter-python '((:pandoc . t)
                                                  (:async . "yes")
                                                  (:session . "py")
                                                  (:kernel . "python3")))
  :config
  (org-babel-do-load-languages
   'org-babel-load-languages
   (add-to-list 'org-babel-load-languages '(jupyter . t)))
  (org-babel-jupyter-override-src-block "python"))

(use-package pyvenv-auto
  :defer t
  :hook ((python-mode . pyvenv-auto-run)))

(cl-defun pspmacs/pip (cmd packages &key (flags nil))
  "Wrapper around pip install working in current virtual environment.

CMD is pip (sub-)command (install, uninstall, etc) to execute.
FLAGS may be string or symbol list of flags passed to subcommand.
PACKAGES are string or symbol list of flags passed to subcommand.
We assume pip = pip3 *always* (python2 is already in antiquity)."
  (let*
      ((pip (or (executable-find "pip") (executable-find "pip3")))
       (cmd (if (symbolp cmd) (symbol-name cmd) cmd))
       (packages (mapcar (lambda (x)
                           (if (symbolp x) (symbol-name x) x))
                         packages))
       (flags (mapcar (lambda (x) (if (symbolp x) (symbol-name x) x)) flags))
       (sh-args (remq nil `(,cmd ,@flags ,@packages))))
    (switch-to-buffer-other-window "*pip*")
    (apply 'start-process "pip" "*pip*" pip sh-args)))

(defun pspmacs/pip-interactive ()
  "Handle python pip interactively in current virtualenv

ACTION: action to perform (install, uninstall)"
  (interactive)
  (let ((action (completing-read "Action: "
                                 '("install" "uninstall" "arbitrary"))))
    (cond ((member action '("uninstall" "install"))
           (let ((packages (split-string (read-string "Packages: ")))
                 (flags (split-string (read-string "Flags: "))))
             (pspmacs/pip action packages :flags flags)))
          (t (let ((cmd (read-string "sub-command: "))
                   (args (split-string (read-string "arguments and flags: "))))
               (pspmacs/pip cmd args))))))

(use-package importmagic
  :defer t
  :general
  (pspmacs/local-leader-keys
    :states 'normal
    :keymaps 'importmagic-mode-map
    "i"  '(:ignore t :wk "import")
    "if" '(importmagic-fix-symbol-at-point :wk "fix at point")
    "ii" '(importmagic-fix-imports :wk "fix")
    "iI" '(importmagic-fix-symbol :wk "manually"))
  :hook
  (python-mode . importmagic-mode))

(use-package isortify
  :defer t
  :hook (python-mode . isortify-mode))

(use-package yapfify
  :defer t
  :hook (python-mode . yapf-mode))

(use-package pydoc
  :general
  (pspmacs/leader-keys :keymaps 'python-mode-map
    "d"  '(:ignore t :wk "documentation")
    "d." '(pydoc-at-point :wk "point")
    "d$" '(pydoc-browse :wk "browse")
    "dd" '(pydoc :wk "prompt")))

(use-package numpydoc
  :general
  (pspmacs/leader-keys :keymaps 'python-mode-map
    "d"  '(:ignore t :wk "documentation")
    "di" '(numpydoc-generate :wk "insert"))
  :custom
  (numpydoc-insertion-style 'yas)
  (numpydoc-insert-parameter-types t)
  (numpydoc-insert-examples-block nil))

(use-package flymake-ruff
  :hook
  (python-mode . flymake-ruff-load))

(pspmacs/load-inherit)
;;; pspmacs-python.el ends here

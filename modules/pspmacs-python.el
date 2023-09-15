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
            (pspmacs/inferior-interpreter 'run-python))
          :wk "python"))
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
         (:jedi_completion (:fuzzy t)
                           :jedi (:environment ,venv-directory)
                           :rope (:enabled t)
                           :pyflakes (:enabled t)
                           :mccabe (:enabled t)
                           :pycodestyle (:enabled t)
                           :pydocstyle (:enabled t :convention "google")
                           :yapf (:enabled t)
                           :flake8 (:enabled nil)))))
  :hook
  ((python-mode . pspmacs/prefer-interpreter-ipython)
   (python-mode . pspmacs/prettify-python)
   (python-mode . pspmacs/pyfaces)))

(use-package ein
  :demand t
  :config
  (add-to-list 'org-babel-load-languages '(ein . t)))

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

(use-package pydoc
  :general
  (pspmacs/leader-keys :keymap 'python-mode-map
    "d"  '(:ignore t :wk "describe")
    "d." '(pydoc-at-point :wk "this")
    "d$" '(pydoc-browse :wk "browse")
    "dd" '(pydoc :wk "prompt")))

(pspmacs/load-inherit)
;;; pspmacs-python.el ends here

;;; pspmacs-prog.el --- common programming config -*- lexical-binding: t; -*-

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

  :init
  (setq lsp-restart 'ignore)
  (setq lsp-session-file (expand-file-name
              ".lsp-session-v1" xdg/emacs-cache-directory))
  ;; (setq lsp-eldoc-enable-hover nil)
  ;; (setq lsp-enable-file-watchers nil)
  ;; (setq lsp-signature-auto-activate nil)
  ;; (setq lsp-modeline-diagnostics-enable nil)
  ;; (setq lsp-keep-workspace-alive nil)
  ;; (setq lsp-auto-execute-action nil)
  ;; (setq lsp-before-save-edits nil)
  ;; (setq lsp-headerline-breadcrumb-enable nil)
  ;; (setq lsp-diagnostics-provider :none)
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
   ;; (setq lsp-ui-doc-show-with-mouse nil)
   (setq lsp-ui-doc-show-with-cursor t)
   (setq lsp-ui-peek-always-show t)
   (setq lsp-ui-peek-fontify 'always)
   :config
   (setq lsp-ui-doc-enable t
     lsp-ui-doc-delay 1)
   :hook (lsp-mode . lsp-ui-mode))

(use-package display-fill-column-indicator
  :demand t
  :hook
  (prog-mode . display-fill-column-indicator-mode)
  :init
  (setq-default fill-column 80))

(use-package eldoc
  :hook (emacs-lisp-mode cider-mode))

(use-package dap-mode
   :hook
   ((dap-terminated . pspmacs/hide-debug-windows)
    (dap-session-created . (lambda (_arg) (projectile-save-project-buffers)))
    (dap-ui-repl-mode . (lambda () (setq-local truncate-lines t))))

   :general
   (pspmacs/local-leader-keys
     :states '(normal)
     :keymaps '(python-mode-map dap-ui-repl-mode-map)
     ;; "d t" '(pspmacs/dap-dtale-df :wk "dtale df")
     "d d" '(dap-debug :wk "debug")
     "d b" '(dap-breakpoint-toggle :wk "breakpoint toggle")
     "d B" '(dap-ui-breakpoints-list :wk "breakpoint list")
     "d c" '(dap-continue :wk "continue")
     "d n" '(dap-next :wk "next")
     "d e" '(dap-eval-thing-at-point :wk "eval")
     "d i" '(dap-step-in :wk "step in")
     "d l" '(dap-debug-last :wk "step in")
     "d q" '(dap-disconnect :wk "quit")
     "d r" '(dap-ui-repl :wk "repl")
     "d h" '(dap-hydra :wk "hydra")
     "d i" '(pspmacs/dap-inspect-df :wk "view df")
     "d I" '(pspmacs/dap-inspect-df2 :wk "view df2"))

   (:keymaps 'dap-ui-repl-mode-map
         "<backtab>" 'dabbrev-completion
         "TAB" 'pspmacs/py-indent-or-complete)
   :init
   ;; (defun pspmacs/dap-dtale-df (dataframe)
   ;;   "Show df in tale in default browser"
   ;;   (interactive (list (read-from-minibuffer "DataFrame: " (evil-find-symbol nil))))
   ;;   (dap-eval (concat "import dtale; dtale.show(" dataframe ", open_browser=True)")))
   (setq pspmacs/dap-temp-dataframe-buffer  "*inspect-df*")
   (setq pspmacs/dap-temp-dataframe-path "~/tmp-inspect-df.csv")

   (defun pspmacs/dap-inspect-df (dataframe)
     "Save the df to csv and open the file with csv-mode"
     (interactive (list (read-from-minibuffer "DataFrame: " (evil-find-symbol nil))))
     (dap-eval (format  "%s.to_csv('%s', index=False)" dataframe pspmacs/dap-temp-dataframe-path))
     (sleep-for 1)
     (find-file-other-window pspmacs/dap-temp-dataframe-path))

   (defun pspmacs/dap-inspect-df2 (dataframe)
     "Save the df to csv and open the file with csv-mode"
     (interactive (list (read-from-minibuffer "DataFrame: " (evil-find-symbol nil))))
     (dap-eval (concat dataframe ".to_csv('~/tmp-inspect-df2.csv', index=False)"))
     (sleep-for 1)
     (with-current-buffer 
     (display-buffer
      (with-current-buffer (find-file-noselect "~/tmp-inspect-df2.csv")
        (rename-buffer "*inspect-df2*"))
      '((;display-buffer-in-side-window
         display-buffer-reuse-window)
        (side . right)
        (window-width . 0.5)))))

   ;; prevent minibuffer prompt about reloading from disk
   (setq revert-without-query '("~/tmp-inspect-df.csv"))
   ;; (setq dap-auto-configure-features '(locals repl))
   (setq dap-auto-configure-features '(sessions repl))
   (setq dap-python-debugger 'debugpy)
   ;; show stdout
   (setq dap-auto-show-output t)
   (setq dap-output-window-min-height 10)
   (setq dap-output-window-max-height 200)
   (setq dap-overlays-use-overlays nil)
   ;; hide stdout window  when done

   (defun pspmacs/hide-debug-windows (session)
     "Hide debug windows when all debug sessions are dead."
     (unless (-filter 'dap--session-running (dap--get-sessions))
   ;; delete output buffer
   (when-let (window (get-buffer-window (dap--debug-session-output-buffer (dap--cur-session-or-die))))
     (delete-window window))
   ;; delete dataframe inspector window
   (when-let
       (window (get-buffer-window (get-file-buffer pspmacs/dap-temp-dataframe-path)))
     (delete-window window))))

   (defun pspmacs/dap-python--executable-find (orig-fun &rest args)
     (executable-find "python"))

   :config
   ;; configure windows
   (require 'dap-ui)
   (setq dap-ui-buffer-configurations
     '(("*dap-ui-sessions*"
        (side . bottom)
        (slot . 1)
        (window-height . 0.33))
       ("*debug-window*"
        (side . bottom)
        (slot . 2)
        (window-height . 0.33))
       ("*dap-ui-repl*"
        (side . bottom)
        (slot . 3)
        (window-height . 0.33))))
   (dap-ui-mode 1)
   ;; python virtualenv
   (require 'dap-python)
   (advice-add 'dap-python--pyenv-executable-find :around #'pspmacs/dap-python--executable-find)
   ;; debug templates
   (defvar dap-script-args (list :type "python"
                 :args []
                 :cwd "${workspaceFolder}"
                 :justMyCode :json-false
                 :request "launch"
                 :debugger 'debugpy
                 :name "dap-debug-script"))
   (defvar dap-test-args (list :type "python-test-at-point"
               :args ""
               :justMyCode :json-false
               ;; :cwd "${workspaceFolder}"
               :request "launch"
               :module "pytest"
               :debugger 'debugpy
               :name "dap-debug-test-at-point"))
   (defvar flight-tower-mill (list
                  :name "mill"
                  :type "python"
                  :request "launch"
                  :program (expand-file-name "~/git/Sodra.Common.FlightTower/flight_tower/__main__.py")
                  ;; :env '(("NO_JSON_LOG" . "true"))
                  :args ["-m" "mill" "--config" "user_luca"]))
   (defvar flight-tower-calibration (list
                     :name "mill"
                     :type "python"
                     :request "launch"
                     :program (expand-file-name "~/git/Sodra.Common.FlightTower/flight_tower/__main__.py")
                     ;; :env '(("NO_JSON_LOG" . "true"))
                     :args ["-m" "mill"
                        ;; "--config" "user_luca"
                        ;; "--config" "calibration_g292imp_41x185"
                        ;; "--config" "calibration_41x185_38x89"
                        "--config" "calibration_jan22"]
                     ))
   (defvar flight-tower-e2e (list
                 :name "mill"
                 :type "python"
                 :request "launch"
                 :program (expand-file-name "~/git/Sodra.Common.FlightTower/flight_tower/__main__.py")
                 ;; :env '(("NO_JSON_LOG" . "true"))
                 :args ["-m" "wood_processing_e2e"
                    "--config" "user_luca"]))
   (dap-register-debug-template "dap-debug-script" dap-script-args)
   (dap-register-debug-template "dap-debug-test-at-point" dap-test-args)
   (dap-register-debug-template "flight-tower-mill" flight-tower-mill)
   (dap-register-debug-template "flight-tower-e2e" flight-tower-e2e)
   (dap-register-debug-template "flight-tower-calibration" flight-tower-calibration)

   ;; bind the templates
   (pspmacs/local-leader-keys
     :keymaps 'python-mode-map
     "d t" '((lambda () (interactive) (dap-debug dap-test-args)) :wk "test")
     "d s" '((lambda () (interactive)
       (dap-debug dap-script-args))
:wk "script")))

(pspmacs/load-inherit)
(provide 'pspmacs-prog)
;;; prog.el ends here

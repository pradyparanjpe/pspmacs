;;; javascript.el --- javascript ide -*- lexical-binding: t; -*-

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

(defun pspmacs/ensure-webpack ()
  "Ensure existance of 'webpack.js'.

Else, offer to trigger initialize."
  (interactive)
  (catch 'abort
    (let ((default-directory
           (or (project-root (project-current)) default-directory)))
      (when (not (file-exists-p default-directory))
        (if (y-or-n-p
             (format "Directory %s doesn't exist. Create?" default-directory))
            (make-directory default-directory t)
          (progn
            (message "aborting")
            (throw 'abort))))
      (when (file-exists-p "webpack.config.js")
        (if (y-or-n-p "File 'webpack.config.js' already exists.")
            (delete-file "webpack.config.js")
          (progn
            (message "aborting")
            (throw 'abort))))
      (start-process "initiate webdev" "*init webdev*"
                     "webpack" "init" "-f"))))

(defun pspmacs/ensure-tsconfig ()
  "Ensure existance of 'tsconfig.js'.

Else, offer to trigger initialize."
  (interactive)
  (catch 'abort
    (let ((default-directory
           (or (project-root (project-current)) default-directory)))
      (when (not (or (file-exists-p "webpack.config.js")
                     (file-exists-p "src/")))
        (if (y-or-n-p "Webpack not yet initialized. Init?")
            (call-interactively 'pspmacs/ensure-webpack)))
      (when (file-exists-p "tsconfig.json")
        (if (y-or-n-p "File 'tsconfig.json' already exists. Replace?")
            (delete-file "tsconfig.json")
          (progn
            (message "aborting")
            (throw 'abort))))
      (start-process "initialize typescript" "*initialize typescript*"
                     "tsc" "--init"
                      "--rootDir" "./src"
                      "--target" "es2016"
                      "--jsx" "react"
                      "--module" "es6"
                      "--moduleResolution" "node"
                      "--outDir" "./dist"))))

(use-package typescript-mode
  :after tree-sitter
  :config
  (dolist (webmode '(html css typescript javascript) nil)
    (add-hook
     (intern (format "%s-mode-hook" (symbol-name webmode)))
     (lambda ()
       (progn
         (setq-local compile-command "webpack build ")
         (setq-local pspmacs/serve-or-run-command "webpack-dev-server "))))
    (add-hook
     (intern (format "%s-mode-hook" (symbol-name webmode)))
     (lambda ()
       (let ((default-directory
              (or (project-root (project-current)) default-directory)))
         (unless (file-exists "tsconfig.json")
           (pspmacs/ensure-tsconfig))))
     nil t))
  ;; we choose this instead of tsx-mode so that
  ;; eglot can automatically figure out language for server
  ;; see https://github.com/joaotavora/eglot/issues/624 and
  ;; https://github.com/joaotavora/eglot#handling-quirky-servers
  (define-derived-mode typescriptreact-mode typescript-mode "TypeScript TSX")

  ;; use our derived mode for tsx files
  (add-to-list 'auto-mode-alist '("\\.tsx?\\'" . typescriptreact-mode))
  ;; by default, typescript-mode is mapped to the treesitter typescript parser
  ;; use our derived mode to map both .tsx AND
  ;; .ts -> typescriptreact-mode -> treesitter tsx
  (add-to-list 'tree-sitter-major-mode-language-alist
               '(typescriptreact-mode . tsx)))

(use-package tsi
  :after tree-sitter
  :vc (tsi :url "https://github.com/orzechowskid/tsi.el")
  :commands (tsi-typescript-mode tsi-json-mode tsi-css-mode)
  :init
  (add-hook 'typescript-mode-hook (lambda () (tsi-typescript-mode 1)))
  (add-hook 'json-mode-hook (lambda () (tsi-json-mode 1)))
  (add-hook 'css-mode-hook (lambda () (tsi-css-mode 1)))
  (add-hook 'scss-mode-hook (lambda () (tsi-scss-mode 1))))

(use-package apheleia
  :ensure t
  :hook
  (typescript-mode . apheleia-mode))

(use-package ansi-color
  :init
  (defun colorize-compilation-buffer ()
    (ansi-color-apply-on-region compilation-filter-start (point-max)))
  :config
  (add-hook 'compilation-filter-hook 'colorize-compilation-buffer))

(use-package json-mode)

(pspmacs/load-inherit)
;;; pspmacs-javascript.el ends here

;;;; pspmacs-browse.el --- filesystem browser -*- lexical-binding: t; -*-

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

(use-package link-hint
  :general
  (pspmacs/leader-keys "l" '(link-hint-open-link :wk "open link"))
  :config
  (setq browse-url-browser-function 'browse-url-firefox)
  (setq link-hint-avy-style 'pre))

(use-package projectile
  :general
  ;; window bindings
  (pspmacs/leader-keys
    :states 'normal
    "p" '(:ignore t :keymap projectile-command-map :which-key "projectile")
    "p <escape>" 'keyboard-escape-quit
    "p a" '(projectile-add-known-project :wk "add known")
    "p F" '(pspmacs/projectile-find-file-all :wk "find file (all)")
    "p t" '(projectile-run-vterm :wk "term"))
  :init
  (when (file-directory-p "~/git")
    (setq projectile-project-search-path '("~/git")))
  (setq projectile-completion-system 'default)
  (setq projectile-known-projects-file
    (expand-file-name "projectile-bookmarks.tld" xdg/emacs-cache-directory))
  (setq projectile-project-root-files
    '(".envrc" ".projectile" "project.clj" "deps.edn"))
  (setq projectile-switch-project-action 'projectile-commander)

  ;; Do not include straight repos (emacs packages) to project list
  (setq
   projectile-ignored-project-function
   (lambda (project-root)
     (string-prefix-p
  (expand-file-name "straight/" user-emacs-directory) project-root)))

  (defun pspmacs/projectile-find-file-all ()
    (interactive)
    (let ((projectile-git-command "git ls-files -zco"))
  (projectile-find-file)))

  :config
  (defadvice projectile-project-root (around ignore-remote first activate)
    (unless (file-remote-p default-directory) ad-do-it))
  (projectile-mode)
  ;; projectile commander methods
  (setq projectile-commander-methods nil)
  (def-projectile-commander-method ??
    "Commander help buffer."
    (ignore-errors (kill-buffer projectile-commander-help-buffer))
    (with-current-buffer
    (get-buffer-create projectile-commander-help-buffer)
  (insert "Projectile Commander Methods:\n\n")
  (dolist (met projectile-commander-methods)
    (insert (format "%c:\t%s\n" (car met) (cadr met))))
  (goto-char (point-min))
  (help-mode)
  (display-buffer (current-buffer) t))
    (projectile-commander))
  (def-projectile-commander-method ?t
    "Open a *shell* buffer for the project."
    (projectile-run-vterm))
  (def-projectile-commander-method ?\C-? ;; backspace
    "Go back to project selection."
    (projectile-switch-project))
  (def-projectile-commander-method ?d
    "Open project root in dired."
    (projectile-dired))
  (def-projectile-commander-method ?f
    "Find file in project."
    (projectile-find-file))
  (def-projectile-commander-method ?g
    "Git status in project."
    (projectile-vc)))

(setq backup-directory-alist
      `((".*" . ,(expand-file-name "backups" xdg/emacs-data-directory))))
(setq auto-save-file-name-transforms
      `((".*" ,(expand-file-name "backups" xdg/emacs-data-directory) t)))

(use-package restart-emacs
  :general
  (pspmacs/leader-keys
    "qr" '(restart-emacs :wk "and restart")))

(pspmacs/load-inherit)
(provide 'pspmacs-browsers)

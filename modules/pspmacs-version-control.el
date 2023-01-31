;;; pspmacs-version-control.el --- git it -*- lexical-binding: t; -*-

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

(use-package magit
  :general
  (pspmacs/leader-keys
   "g" '(:ignore t :kw "git")
   "g b" 'magit-blame
   "g s" 'magit-status
   "g S" 'magit-status-here
   "g l" 'magit-log)
  (general-nmap
    :keymaps '(magit-status-mode-map
               magit-stash-mode-map
               magit-revision-mode-map
               magit-process-mode-map
               magit-diff-mode-map)
    "TAB" #'magit-section-toggle
    "<escape>" #'transient-quit-one)
  :init
  (setq git-commit-fill-column 50)
  (setq transient-history-file
        (expand-file-name "transient/history.el" xdg/emacs-cache-directory))
  (setq transient-values-file
        (expand-file-name "transient/values.el" xdg/emacs-cache-directory))
  (setq transient-levels-file
        (expand-file-name "transient/levels.el" xdg/emacs-cache-directory))
  :custom
  (magit-display-buffer-function
   #'magit-display-buffer-same-window-except-diff-v1)
  (magit-log-arguments '("--graph" "--decorate" "--color"))
  ;; (magit-log-margin (t "%Y-%m-%d %H:%M " magit-log-margin-width t 18))

  :config
  (evil-define-key* '(normal visual) magit-mode-map
    "zz" #'evil-scroll-line-to-center))

(use-package diff-hl
  :config
  (global-diff-hl-mode))

(pspmacs/load-inherit)

;;; pspmacs-version-control.el ends here
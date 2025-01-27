;;; pspmacs-version-control.el --- git it -*- lexical-binding: t; -*-

;; Copyright © 2023-2024  Pradyumna Swanand Paranjape

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

(use-package transient
  :custom
  (transient-history-file (xdg/make-path "transient/history.el" 'cache))
  (transient-values-file (xdg/make-path "transient/values.el" 'cache))
  (transient-levels-file (xdg/make-path "transient/levels.el" 'cache)))

(use-package magit
  :after transient
  :init
  (setq magit-define-global-key-bindings nil)
  :general
  (pspmacs/leader-keys
   "g" '(:ignore t :wk "it")
   "gb" '(magit-blame :wk "lame")
   "gl" '(magit-log :wk "og")
   "gm" '(magit-dispatch :wk "enu")
   "gM" '(magit-file-dispatch :wk "enu (file)")
   "gs" '(magit-status :wk "tatus")
   "gS" '(magit-status-here :wk "tatus here"))
  (general-nmap
    :keymaps '(magit-status-mode-map
               magit-stash-mode-map
               magit-revision-mode-map
               magit-process-mode-map
               magit-diff-mode-map)
    "TAB" #'magit-section-toggle
    "<escape>" #'transient-quit-one)
  :custom
  (magit-display-buffer-function
   #'magit-display-buffer-same-window-except-diff-v1)
  (magit-log-arguments '("--graph" "--decorate" "--color"))
  (git-commit-major-mode 'gfm-mode)
  :hook
  ((git-commit-setup . bug-reference-mode)
   (git-commit-setup . (lambda () (setq-local fill-column 50)))
   (git-commit-setup . display-fill-column-indicator-mode)))
;; (magit-log-margin (t "%Y-%m-%d %H:%M " magit-log-margin-width t 18))

(use-package ediff
  :custom
  (ediff-keep-variants nil)
  (ediff-make-buffers-readonly-at-startup nil)
  (ediff-split-window-function #'split-window-horizontally)
  (ediff-window-setup-function #'ediff-setup-windows-plain))

(use-package diff-hl
  :demand t
  :general
  (pspmacs/leader-keys
    "gd" '(:ignore t :wk "iff")
    "gd<" '(diff-hl-revert-hunk :wk ":Revert")
    "gd>" '(diff-hl-stage-current-hunk :wk ":Stage")
    "gdd" '(diff-hl-show-hunk :wk ":See")
    "gdn" '(diff-hl-next-hunk :wk "ext")
    "gdN" '(diff-hl-show-hunk-next :wk "ext & see")
    "gdp" '(diff-hl-previous-hunk :wk "rev")
    "gdP" '(diff-hl-show-hunk-previous :wk "rev & see"))
  :custom
  (diff-hl-disable-on-remote t)
  :hook
  (magit-pre-refresh . diff-hl-magit-pre-refresh)
  (magit-post-refresh . diff-hl-magit-post-refresh)
  :config
  (unless (display-graphic-p)
    (diff-hl-margin-mode))
  (diff-hl-flydiff-mode)
  (global-diff-hl-mode))

(use-package emacs
  :custom
  (auto-revert-check-vc-info t))

(pspmacs/load-inherit)

;;; pspmacs-version-control.el ends here

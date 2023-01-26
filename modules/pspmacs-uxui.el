;;; pspmacs-uxui.el --- User experience/interface -*- lexical-binding: t; -*-

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

(use-package emacs
  :init
  ;;; locale
  (setq locale-coding-system 'utf-8)
  (setq coding-system-for-read 'utf-8)
  (setq coding-system-for-write 'utf-8)
  (setq default-process-coding-system '(utf-8-unix . utf-8-unix))
  (set-terminal-coding-system 'utf-8)
  (set-keyboard-coding-system 'utf-8)
  (set-selection-coding-system 'utf-8)
  (prefer-coding-system 'utf-8)
  (set-default-coding-systems 'utf-8)

  ;;; UI
  (setq display-time-24hr-format t)
  (customize-set-variable 'large-file-warning-threshold (* 100 1000 1000))
  (show-paren-mode t)
  (global-hl-line-mode 1)
  (column-number-mode t)
  (display-fill-column-indicator-mode)
  (display-time-mode)

  ;;; Font
  (set-face-attribute 'default nil :font "Fira Code" :height 150)
  (global-set-key (kbd "C-=") 'text-scale-increase)
  (global-set-key (kbd "C--") 'text-scale-decrease)

  ;;; auto-complete
  ;; tabs
  (setq-default indent-tabs-mode nil
        tab-width 4))

;;; Garbage collector
(use-package gcmh
  :demand t
  :config
  (gcmh-mode 1))

(use-package all-the-icons)

;; prettify dired with icons
(use-package all-the-icons-dired
  :hook
  (dired-mode . all-the-icons-dired-mode))

(use-package dashboard
  :demand t
  :diminish dashboard-mode
  :general
  (pspmacs/leader-keys
    "bh" '(pspmacs/home-splash :which-key "home splash"))
  :init
  (setq dashboard-set-footer nil)
  (defun pspmacs/home-splash ()
    "Visit home screen"
    (interactive)
    (progn
  (switch-to-buffer (get-buffer-create "*dashboard*"))
  (dashboard-refresh-buffer)))
  :config
  (setq dashboard-startup-banner
    (expand-file-name "data/Tux.svg" user-emacs-directory))
  (setq dashboard-banner-logo-title
    "Prady' Structured, Personalized Emacs")
  (setq dashboard-items '((projects . 2)
              (recents . 5)
              (agenda . 5)))
  (setq dashboard-center-content t)
  (dashboard-setup-startup-hook)
  (setq initial-buffer-choice (lambda () (pspmacs/home-splash))))

(use-package electric
  :demand t
  :hook
  ;; disable electric indentation in org-mode
  (org-mode . (lambda () (electric-indent-local-mode -1)))
  :init
  (electric-pair-mode +1) ;; automatically insert closing parens
  (setq electric-pair-preserve-balance nil)) ;; more annoying than useful

(use-package solaire-mode
  :config
  (solaire-global-mode +1))

(use-package doom-themes
  :config
  ;; Global settings (defaults)
  (setq doom-themes-enable-bold t ; if nil, bold is universally disabled
    doom-themes-enable-italic t)
  ; if nil, italics is universally disabled
  (load-theme 'doom-one t)

  ;; Enable flashing mode-line on errors
  (doom-themes-visual-bell-config)

  ;; Corrects (and improves) org-mode's native fontification.
  (doom-themes-org-config))

(use-package doom-modeline
  :demand t
  :init
  (setq doom-modeline-env-version t)
  (setq doom-modeline-buffer-encoding nil)
  (setq doom-modeline-height 15)
  (setq doom-modeline-project-detection 'projectile)
  (setq doom-modeline-icon t)
  :config
  (doom-modeline-mode 1)
  (set-face-attribute 'mode-line nil
          :background "#050614"
          :foreground "white"
          :box '(:line-width 8 :color "#050614")
          :overline nil
          :underline nil)
  (set-face-attribute 'mode-line-inactive nil
          :background "#262033"
          :foreground "white"
          :box '(:line-width 8 :color "#262033")
          :overline nil
          :underline nil)
  (set-face-attribute 'doom-modeline-evil-insert-state nil
          :foreground "green")
  (set-face-attribute 'doom-modeline-evil-normal-state nil
          :foreground "orange")
  (set-face-attribute 'doom-modeline-evil-replace-state nil
          :foreground "yellow")
  (set-face-attribute 'doom-modeline-evil-visual-state nil
          :foreground "cyan"))

 (straight-use-package
  '(space-theming :host github :repo "p3r7/space-theming"))

 (use-package space-theming
   :init
   (setq-default
    space-theming-modifications
    '((doom-one
   (font-lock-defaults :background "#000307" :foreground "#959a9f")
   (default :background "#000307" :foreground "#959a9f")
   (hl-line :slant italic)
   (org-emph-face :slant italic)
   (highlight :background "#3f3f5f" :underline t)
   (font-lock-rpack-face :foreground "#9f7fff")
   (font-lock-relem-face :foreground "#bf8faf")
   (font-lock-rsuper-face :foreground "#8fafbf")
   (font-lock-comment-face :foreground "#3f4f5f" :background "#0f0f0f")
   (font-lock-constant-face :foreground "#af1f1f")
   (font-lock-builtin-face :foreground "#d76f10")
   (font-lock-doc-face :foreground "#875f3f")
   (font-lock-string-face :forground "#1faf5f")
   (font-lock-variable-name-face :foreground "#ffffaf")
   (font-lock-function-name-face :foreground "#9f5f9f" :weight bold)
   (font-lock-type-face :foreground "#ff3f5f" :weight bold))))
   (setq custom--inhibit-theme-enable nil)

   :config
   (space-theming-init-theming)
   (setq space-theming--current-theme 'doom-one)
   (space-theming-update-current-theme))

(use-package hl-todo
  :init
  (setq hl-todo-keyword-faces
    '(("FAIL"  .  "#ff3f3f")
      ("FIXME" .  "#ff6f3f")
      ("TEMP"  .  "#ff9f3f")
      ("HACK"  .  "#ffcf3f")
      ("TODO"  .  "#ffff3f")
      ("LAZY"  .  "#e7ff3f")
      ("WAIT"  .  "#cfff3f")
      ("NEXT"  .  "#9fff3f")
      ("ALGO"  .  "#6fff3f")
      ("PROG"  .  "#3fff3f")
      ("TEST"  .  "#3fe757")
      ("ACTS"  .  "#3fcf6f")
      ("SENT"  .  "#3f9f9f")
      ("OKAY"  .  "#3f6fcf")
      ("DONE"  .  "#3f3fff")
      ("NOTE"  .  "#ffcf6f")
      ("XXXX"  .  "#ff9f9f")
      ("DONT"  .  "#ff6fcf")
      ("CANT"  .  "#ff3fff")))
  (global-hl-todo-mode))

(use-package helm
  :demand t
  :bind (("M-x" . helm-M-x))
  :general
  (general-define-key
   :keymaps 'helm-map
   "TAB" #'helm-execute-persistent-action
   "C-z" #'helm-select-action)
  (pspmacs/leader-keys
    "SPC" '(helm-M-x :wk "helm-M-x"))
  (pspmacs/leader-keys
    "ff" '(helm-find-files :wk "find files")
    "bb" '(helm-buffers-list :wk "switch buffer"))
  :config
  (helm-mode 1)
  (setq read-extended-command-predicate
        #'command-completion-default-include-p))

(use-package rainbow-mode
  :hook ((prog-mode . rainbow-mode)
     (org-mode . rainbow-mode)
     (emacs-lisp-mode . rainbow-mode)))

(use-package rainbow-delimiters
  :hook ((emacs-lisp-mode . rainbow-delimiters-mode)
     (clojure-mode . rainbow-delimiters-mode)))

(pspmacs/load-inherit)
(provide 'pspmacs-uxui)
;;; uxui.el ends here

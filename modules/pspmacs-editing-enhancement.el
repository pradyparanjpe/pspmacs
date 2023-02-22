;;; pspmacs-editing-enhancement.el --- writing aid -*- lexical-binding: t; -*-

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

(use-package rainbow-mode
  :hook (((prog-mode org mode) . rainbow-mode)))

(use-package rainbow-delimiters
  :hook (((prog-mode org-mode) . rainbow-delimiters-mode)))

(use-package whitespace
  ;; gratefully borrowed from
  ;; https://www.reddit.com/r/emacs/comments/2keh6u/show_tabs_and_trailing_whitespaces_only/
  :init
  (setq whitespace-display-mappings
        ;; all numbers are Unicode codepoint in decimal.
        ;; try (insert-char 182 ) to see it
        '(
          ;; 32 SPACE, 183 MIDDLE DOT 「·」, 46 FULL STOP 「.」
          (space-mark 32 [183] [46])
          ;; 10 LINE FEED
          (newline-mark 10 [182 10])
          ;; 9 TAB, 9655 WHITE RIGHT-POINTING TRIANGLE 「▷」
          (tab-mark 9 [187 9] [9655 9] [92 9]))
        whitespace-style '(face tabs trailing tab-mark))
  :config
  (set-face-attribute 'whitespace-tab nil
                      :background "#f0f0f0"
                      :foreground "#00a8a8"
                      :weight 'bold)
  (set-face-attribute 'whitespace-trailing nil
                      :background "#ffffff"
                      :foreground "#183bc8"
                      :weight 'normal)
  :hook
  ((prog-mode org-mode) . whitespace-mode))

(use-package whitespace-cleanup-mode
  :hook (prog-mode . whitespace-cleanup))

(use-package all-the-icons)

;; prettify dired with icons
(use-package all-the-icons-dired
  :if (display-graphic-p)
  :hook
  (dired-mode-map . (lambda () (interactive)
                      (unless (file-remote-p default-directory)
                        (all-the-icons-dired-mode)))))

(use-package multiple-cursors
  :after evil
  :general
  (pspmacs/leader-keys
    "s" '(:ignore t :wk "multiple-substitute")
    "se" '(:ignore t :state 'visual :wk "edit")
    :states 'visual
    "sef" '(mc/mark-all-like-this-in-defun :wk "function")
    "seb" '(mc/mark-all-like-this :wk "buffer"))
  (pspmacs/leader-keys
    :states 'normal
    "sef" '(mc/mark-all-symbols-like-this-in-defun :wk "function")
    "seb" '(mc/mark-all-symbols-like-this :wk "buffer")))

(use-package smartparens
  :custom
  (sp-show-pair-from-inside nil)
  (show-paren-mode t)
  (smartparens-global-mode t)
  :config
  (sp-local-pair 'python-mode "\"\"\"" "\"\"\"")
  (sp-local-pair 'python-mode "__" "__")
  (sp-with-modes 'emacs-lisp-mode-map
    ;; disable ', it's the quote character!
    (sp-local-pair "'" nil :actions nil)
    ;; also only use the pseudo-quote inside strings where it
    ;; serves as hyperlink.
    (sp-local-pair "`" "'" :when '(sp-in-string-p sp-in-comment-p))))

(use-package undo-tree
  :general
  (general-define-key
   :keymaps 'evil-normal-state-map
   "u" #'undo-tree-undo
   "C-r" #'undo-tree-redo)
  :init
  (global-undo-tree-mode)
  :config
  (mkdir (expand-file-name "undo-tree/" xdg/emacs-cache-directory) t)
  :custom
  (undo-tree-auto-save-history t)
  (undo-tree-history-directory-alist
   `((".*" . ,(expand-file-name "undo-tree/" xdg/emacs-cache-directory))))
  (undo-tree-visualizer-diff t)
  (undo-tree-visualizer-timestamps t))

(use-package yasnippet
  :general
  (pspmacs/leader-keys
    "y" '(:ignore t :wk "yas")
    "yn" '(yas-new-snippet :wk "new")
    "yi" '(yas-insert-snippet :wk "insert"))
  (yas-minor-mode-map
   :states 'insert
   "TAB" 'nil
   "C-TAB" 'yas-expand)
  :config
  (pspmacs/extend-list
   'yas-snippet-dirs
   (mapcar
    (lambda (x) (expand-file-name "snippets" x)) pspmacs/worktrees))
  (dolist (snippets-wt yas-snippet-dirs nil)
    (mkdir snippets-wt t))
  (yas-reload-all)
  :hook
  (((prog-mode org-mode) . yas-minor-mode)))

(general-add-hook 'org-mode-hook 'flyspell-mode)
(pspmacs/leader-keys
  "S" '(:ignore t :wk "flyspell")
  "Sb" '(flyspell-buffer :wk "next")
  "Sn" '(evil-next-flyspell-error :wk "next")
  "Sp" '(evil-prev-flyspell-error :wk "previous"))

(use-package emacs
  :config
  (setq-default display-line-numbers-type 'relative)
  (global-display-line-numbers-mode 1))

(pspmacs/load-inherit)
;;; pspmacs-editing-enhancement.el ends here

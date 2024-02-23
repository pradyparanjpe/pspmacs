;;; pspmacs-editing-enhancement.el --- writing aid -*- lexical-binding: t; -*-

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

(use-package rainbow-mode
  :hook (((prog-mode org-mode) . rainbow-mode)))

(use-package rainbow-delimiters
  :hook ((prog-mode . rainbow-delimiters-mode)))

(use-package whitespace
  ;; gratefully borrowed from
  ;; https://www.reddit.com/r/emacs/comments/2keh6u/show_tabs_and_trailing_whitespaces_only/
  :custom
  (whitespace-display-mappings
   ;; all numbers are Unicode codepoint in decimal.
   ;; try (insert-char 182 ) to see it
   '(
     ;; 32 SPACE, 183 MIDDLE DOT 「·」, 46 FULL STOP 「.」
     (space-mark 32 [183] [46])
     ;; 10 LINE FEED
     (newline-mark 10 [182 10])
     ;; 9 TAB, 9655 WHITE RIGHT-POINTING TRIANGLE 「▷」
     (tab-mark 9 [187 9] [9655 9] [92 9])))
  (whitespace-style '(face tabs trailing tab-mark))
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

(use-package all-the-icons
  :if (display-graphic-p))

;; prettify dired with icons
(use-package all-the-icons-dired
  :if (display-graphic-p)
  :after all-the-icons
  :hook
  (dired-mode . (lambda () (interactive)
                  (unless (file-remote-p default-directory)
                    (all-the-icons-dired-mode)))))

(use-package evil-mc
  :after evil
  :config
  (global-evil-mc-mode 1))

(use-package undo-tree
  :general
  (general-define-key
   :keymaps 'evil-normal-state-map
   "u" #'undo-tree-undo
   "C-r" #'undo-tree-redo)
  :init
  (global-undo-tree-mode)
  :config
  (mkdir (xdg/make-path "undo-tree/" 'cache) t)
  :custom
  (undo-tree-auto-save-history t)
  (undo-tree-history-directory-alist
   `((".*" . ,(xdg/make-path "undo-tree/" 'cache))))
  (undo-tree-visualizer-diff t)
  (undo-tree-visualizer-timestamps t))

(use-package yasnippet-snippets
  :after yasnippet)

(use-package yasnippet
  :pin melpa
  :general
  (pspmacs/leader-keys
    "y" '(:ignore t :wk "as")
    "yn" '(yas-new-snippet :wk "new")
    "yi" '(yas-insert-snippet :wk "insert"))
  (yas-minor-mode-map
   :states 'insert
   "TAB" 'nil
   "C-TAB" 'yas-expand)
  :config
  (let ((pspmacs/snippets (mapcar (lambda (x) (expand-file-name "snippets" x))
                                  pspmacs/worktrees)))
    (dolist (snippets-wt pspmacs/snippets nil) (mkdir snippets-wt t))
    (pspmacs/extend-list 'yas-snippet-dirs pspmacs/snippets))
  (yas-reload-all)
  :hook
  (((prog-mode org-mode) . yas-minor-mode)))

(general-add-hook 'org-mode-hook 'flyspell-mode)
(pspmacs/leader-keys
  "S" '(:ignore t :wk "pell")
  "Sb" '(flyspell-buffer :wk "buffer")
  "Sn" '(evil-next-flyspell-error :wk "next")
  "Sp" '(evil-prev-flyspell-error :wk "previous")
  "Ss" '(flyspell-correct-word-before-point :wk "Menu"))

(use-package smartparens
  :general
  (pspmacs/leader-keys
    "(" '(:ignore t :wk "[ ]")
    "( <backspace>" '(sp-unwrap-sexp :wk "wrap unwrap"))

  :custom
  (sp-show-pair-from-inside nil)
  (show-paren-mode t)
  (smartparens-global-mode t)

  :config
  (let ((paren-bindings
         (mapcan
          (lambda (wrapper)
            (let ((pair-open wrapper)
                  (pair-close (plist-get (sp-get-pair wrapper) :close)))
              `(,(format "(%s" wrapper)
                 '((lambda (&optional arg)
                     (interactive "P")
                     (sp-wrap-with-pair ,pair-open))
                   :wk ,(format "%s%s" pair-open pair-close)))))
          '("(" "[" "{" "\"" "'"))))
    (eval `(pspmacs/leader-keys ,@paren-bindings)))

  (sp-with-modes 'python-mode
    ;; multi-line strings
    (sp-local-pair "'''" "'''" :unless '(sp-point-after-word-p))
    (sp-local-pair "\"\"\"" "\"\"\"" :unless '(sp-point-after-word-p))
    ;; dunder
    (sp-local-pair "__" "__" :unless '(sp-point-after-word-p)))

  (sp-with-modes 'emacs-lisp-mode
    ;; disable ', it's the quote character.
    (sp-local-pair "'" nil :actions nil)
    ;; also only use the pseudo-quote inside strings where it
    ;; serves as hyperlink.
    (sp-local-pair "`" "'" :when '(sp-in-string-p sp-in-comment-p))))

(use-package smog
  :init
  (defun pspmacs/readability (&optional buffer)
    (interactive)
    (progn
      (if buffer
          (smog-check-buffer)
        (smog-check))
      (switch-to-buffer-other-window "*Readability*")))
  :general
  (pspmacs/local-leader-keys
    :keymaps '(text-mode-map org-mode-map)
    :states 'normal
    "=r" '(:ignore t :wk "readibility")
    "=rr" '(pspmacs/readability :wk "check region")
    "=rb" '((lambda () (interactive) (pspmacs/readability t))
            :wk "check buffer")))

(use-package live-wc
  :demand t
  :vc (live-wc :url "https://gitlab.com/pradyparanjpe/live-wc.git")
  :init (global-live-wc-mode)
  :general (pspmacs/leader-keys "#" '(:keymap live-wc-keymap :wk "wc"))
  :custom
  (live-wc-max-buffer-size 1048576)  ; 1mB
  (live-wc-fraction t)
  (live-wc-line-pos 5))

(use-package abbrev
  :ensure nil
  :custom

  (save-abbrevs 'silently)
  (abbrev-suggest t)
  (abbrev-file-name (xdg/make-path "abbrev_defs" 'state))

  :config
  ;; (quietly-read-abbrev-file)  ; shouldn't be necessary
  ;; For safety, global abbreviations must preferably start with a <comma> ','
  (abbrev-table-put global-abbrev-table
                    :regexp "\\(?:^\\|[\t\s]+\\)\\(?1:,?.*\\)")
  ;; Insert user's name
  (define-abbrev global-abbrev-table ",myname" ""
    (lambda () (insert (format "%s " user-full-name))))
  (define-abbrev global-abbrev-table ",nowdt" ""
    (lambda () (insert (format-time-string "%F %T"))))

  :hook
  (text-mode . abbrev-mode))

(use-package wiki-abbrev
  :commands wiki-abbrev-mode
  :after abbrev
  :vc (wiki-abbrev :url "https://codeberg.org/pradyparanjpe/wiki-abbrev.el"))

(use-package emacs
  :config
  (setq-default display-line-numbers-type 'relative)
  (global-display-line-numbers-mode 1))

(pspmacs/load-inherit)
;;; pspmacs-editing-enhancement.el ends here

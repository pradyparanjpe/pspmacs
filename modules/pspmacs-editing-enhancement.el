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
  :hook ((prog-mode . rainbow-mode)
     (org-mode . rainbow-mode)
     (emacs-lisp-mode . rainbow-mode)))

(use-package rainbow-delimiters
  :hook ((emacs-lisp-mode . rainbow-delimiters-mode)
     (clojure-mode . rainbow-delimiters-mode)))

(use-package whitespace
  :hook (prog-mode . whitespace-mode))

(use-package whitespace-cleanup-mode
  :hook (prog-mode . whitespace-cleanup))

(use-package all-the-icons)

;; prettify dired with icons
(use-package all-the-icons-dired
  :hook
  (dired-mode . all-the-icons-dired-mode))

(setq pspmacs/pretty-alist
      '(("code" . (("\\n" . ?⏎)
                   ("\\t" . ?↹)
                   (">=" . ?≥)
                   ("<=" . ?≤)
                   ("!=" . ?≠)
                   ("==" . ?≅)))
        ("lisp" . (("lambda" . ?λ)))
        ("org" . (("#+setupfile" . ?🛒)
                  ("#+author" . ?🖋)
                  ("#+begin_src" . ?)
                  ("#+end_src" . ?⏎)
                  ("#+email" . ?✉)
                  ("#+language" . ?🗣)
                  ("#+options" . ?🔘)
                  ("#+property" . ?⚙)
                  ("#+results" . ?📜)
                  ("#+startup" . ?)
                  ("#+html_head" . ?)
                  ("#+title" . ?§)
                  ("tangle" . ?🔗)
                  ("[x]" . ?✔)
                  ("[ ]" . ?❌)
                  ("[-]" . ?⏳)))
        ("python" . (("and" . ?∩)
                     ("or" . ?∪)
                     ("->" . ?⇒)))))

(defun pspmacs/mode-prettify (sub-modes)
  "Apply pretiffy mode alist according to active-mode"
  (progn
    (setq prettify-symbols-alist
          (mapcan (lambda (x)
                    (list x `(,(upcase (car x)) . ,(cdr x))))
                  (apply #'append
                         (mapcar
                          (lambda (y)
                            (cdr (assoc y pspmacs/pretty-alist))) sub-modes))))
    (prettify-symbols-mode)))

(use-package multiple-cursors
  :after evil
  :ensure t
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

(use-package yasnippet
  :general
  (yas-minor-mode-map
   :states 'insert
   "TAB" 'nil
   "C-TAB" 'yas-expand)
  :hook
  ((prog-mode org-mode) . yas-minor-mode))

(general-add-hook 'org-mode-hook 'flyspell-mode)
(pspmacs/leader-keys
  "S" '(:ignore t :wk "flyspell")
  "Sb" '(flyspell-buffer :wk "next")
  "Sn" '(evil-next-flyspell-error :wk "next")
  "Sp" '(evil-prev-flyspell-error :wk "previous"))

(pspmacs/load-inherit)

;;; pspmacs-editing-enhancement.el ends here

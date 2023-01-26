;;; pspmacs-steno.el --- writing aid -*- lexical-binding: t; -*-

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

(use-package helpful
  :after evil
  :init
  (setq evil-lookup-func #'helpful-at-point)
  :bind
  ([remap describe-function] . helpful-callable)
  ([remap describe-command] . helpful-command)
  ([remap describe-variable] . helpful-variable)
  ([remap describe-key] . helpful-key))

(use-package company
  :ensure t
  :diminish
  :commands (company-mode company-indent-or-complete-common)
  :config
  (setq company-dabbrev-other-buffers t
    company-dabbrev-code-other-buffers t)
  :hook ((text-mode . company-mode)
     (prog-mode . company-mode)))

(use-package company-quickhelp
  :after company
  :bind (:map company-active-map
      ("C-c ?" . company-quickhelp-manual-begin)))

(use-package company-auctex
  :after (company latex))

(use-package company-math
  :defer t)

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
(provide 'pspmacs-steno)
;;; steno.el ends here

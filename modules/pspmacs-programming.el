;;; programming.el --- common programming config -*- lexical-binding: t; -*-

;; Copyright © 2023-2024  Pradyumna Swanand Paranjape

;; Author: Pradyumna Swanand Paranjape <pradyparanjpe@rediffmail.com>
;; Keywords: help, languages

;; This programmingram is free software; you can redistribute it and/or modify
;; it under the terms of the GNU Lesser General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This programmingram is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU Lesser General Public License for more details.

;; You should have received a copy of the GNU Lesser General Public License
;; along with this programmingram.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;;; Code:

(use-package corfu
  ;; Optional customizations
  :demand t
  :custom
  (corfu-cycle t)                ;; Enable cycling for `corfu-next/previous'
  (corfu-auto t)                 ;; Enable auto completion
  ;; (corfu-separator ?\s)          ;; Orderless field separator
  ;; (corfu-quit-at-boundary nil)   ;; Never quit at completion boundary
  (corfu-quit-no-match t)      ;; Never quit, even if there is no match
  ;; (corfu-preview-current nil)    ;; Disable current candidate preview
  (corfu-preselect 'prompt)      ;; Preselect the prompt
  ;; (corfu-on-exact-match nil)     ;; Configure handling of exact matches
  ;; (corfu-scroll-margin 5)        ;; Use scroll margin

  ;; Enable Corfu only for certain modes.
  ;; :hook ((prog-mode . corfu-mode)
            ;;        (shell-mode . corfu-mode)
            ;;        (eshell-mode . corfu-mode)
            ;;        (org-mode . corfu-mode))
  ;; ;; Recommended: Enable Corfu globally.
  ;; ;; This is recommended since Dabbrev can be used globally (M-/).
  ;; See also `corfu-excluded-modes'.
  :config
  (global-corfu-mode))

(use-package cape
  :demand t
  :init
  ;; Add `completion-at-point-functions', used by `completion-at-point'.
  (add-to-list 'completion-at-point-functions #'cape-dabbrev t)
  (add-to-list 'completion-at-point-functions #'cape-file t)
  (add-to-list 'completion-at-point-functions #'cape-history)
  (add-to-list 'completion-at-point-functions #'cape-keyword)
  (add-to-list 'completion-at-point-functions #'cape-tex)
  (add-to-list 'completion-at-point-functions #'cape-sgml)
  (add-to-list 'completion-at-point-functions #'cape-rfc1345)
  (add-to-list 'completion-at-point-functions #'cape-abbrev)
  (add-to-list 'completion-at-point-functions #'cape-dict)
  (add-to-list 'completion-at-point-functions #'cape-elisp-symbol)
  (add-to-list 'completion-at-point-functions #'cape-line))

(use-package kind-icon
  :demand t
  :after corfu
  :custom
  (kind-icon-default-face 'corfu-default) ; to compute blended backgrounds correctly
    :config
    (add-to-list 'corfu-margin-formatters #'kind-icon-margin-formatter))

(use-package corfu-doc-terminal
  :after corfu-terminal
  :ensure nil
  :vc (corfu-doc-terminal :url "https://codeberg.org/akib/emacs-corfu-doc-terminal")
  :config
  (unless (display-graphic-p)
    (corfu-doc-terminal-mode t)))

(use-package corfu-terminal
  :after corfu
  :ensure nil
  :vc (corfu-terminal :url "https://codeberg.org/akib/emacs-corfu-terminal")
  :config
  (unless (display-graphic-p)
    (corfu-terminal-mode t)))

(use-package ligature
  :config
  ;; Enable the "www" ligature in every possible major mode
  (ligature-set-ligatures 't '("www"))
  ;; Enable traditional ligature support in eww-mode, if the
  ;; `variable-pitch' face supports it
  (ligature-set-ligatures 'eww-mode '("ff" "fi" "ffi"))
  ;; markdown mode
  (ligature-set-ligatures
   'markdown-mode
   `(("=" ,(rx (+ "=") (? (| ">" "<"))))
     ("-" ,(rx (+ "-")))))
  ;; Enable all Cascadia and Fira Code ligatures in programming modes
  (ligature-set-ligatures
   'prog-mode
   '(;; "== === ==== => =| =>>=>=|=>==>> ==< =/=//=// =~"
        ;; =:= =!=
        ("=" (rx (+ (or ">" "<" "|" "/" "~" ":" "!" "="))))
        ;; ;; ;;;
        (";" (rx (+ ";")))
        ;; && &&&
        ("&" (rx (+ "&")))
        ;; !! !!! !. !: !!. != !== !~
        ("!" (rx (+ (or "=" "!" "\." ":" "~"))))
        ;; ?? ??? ?:  ?=  ?.
        ("?" (rx (or ":" "=" "\." (+ "?"))))
        ;; %% %%%
        ("%" (rx (+ "%")))
        ;; "|> ||> |||> ||||> |] |} || ||| |-> ||-||"
        ;; "|->>-||-<<-| |- |== ||=||"
        ;; "|==>>==<<==<=>==//==/=!==:===>"
        ("|" (rx (+ (or ">" "<" "|" "/" ":" "!" "}" "\]"
                        "-" "=" ))))
        ;; \\ \\\ \/
        ("\\" (rx (or "/" (+ "\\"))))
        ;; "++ +++ ++++ +>"
        ("+" (rx (or ">" (+ "+"))))
        ;; ":: ::: :::: :> :< := :// ::="
        (":" (rx (or ">" "<" "=" "//" ":=" (+ ":"))))
        ;; "// /// //// /\ /* /> /===:===!=//===>>==>==/"
        ("/" (rx (+ (or ">"  "<" "|" "/" "\\" "\*" ":" "!"
                        "="))))
        ;; ".. ... .... .= .- .? ..= ..<"
        ("\." (rx (or "=" "-" "\?" "\.=" "\.<" (+ "\."))))
        ;; "-- --- ---- -~ -> ->> -| -|->-->>->--<<-|"
        ("-" (rx (+ (or ">" "<" "|" "~" "-"))))
        ;; "*> */ *)  ** *** ****"
        ("*" (rx (or ">" "/" ")" (+ "*"))))
        ;; www wwww
        ("w" (rx (+ "w")))
        ;; "<> <!-- <|> <: <~ <~> <~~ <+ <* <$ </  <+> <*>"
        ;; "<$> </> <|  <||  <||| <|||| <- <-| <-<<-|-> <->>"
        ;; "<<-> <= <=> <<==<<==>=|=>==/==//=!==:=>"
        ;; "<< <<< <<<<"
        ("<" (rx (+ (or "\+" "\*" "\$" "<" ">" ":" "~"  "!"
                        "-"  "/" "|" "="))))
        ;; ">: >- >>- >--|-> >>-|-> >= >== >>== >=|=:=>>"
        ;; ">> >>> >>>>"
        (">" (rx (+ (or ">" "<" "|" "/" ":" "=" "-"))))
        ;; "#: #= #! #( #? #[ #{ #_ #_( ## ### #####"
        ("#" (rx (or ":" "=" "!" "(" "\?" "\[" "{" "_(" "_"
                     (+ "#"))))
        ;; "~~ ~~~ ~=  ~-  ~@ ~> ~~>"
        ("~" (rx (or ">" "=" "-" "@" "~>" (+ "~"))))
        ;; __ ___ ____ _|_ __|____|_
        ("_" (rx (+ (or "_" "|"))))
        ;; Fira code: 0xFF 0x12
        ("0" (rx (and "x" (+ (in "A-F" "a-f" "0-9")))))
        ;; Fira code:
        "Fl"  "Tl"  "fi"  "fj"  "fl"  "ft"
        ;; The few not covered by the regexps.
        "{|"  "[|"  "]#"  "(*"  "}#"  "$>"  "^="))
  ;; Enables ligature checks globally in all buffers. You can also do it
  ;; per mode with `ligature-mode'.
  (global-ligature-mode t))

(use-package tree-sitter
  :ensure t
  :config
  (global-tree-sitter-mode)
  (add-hook 'tree-sitter-after-on-hook #'tree-sitter-hl-mode))

(use-package tree-sitter-langs
  :ensure t
  :after tree-sitter)

(use-package eglot
  :init
  (add-hook 'orderless-style-dispatchers
            #'pspmacs/orderless-dispatch-flex-first nil 'local)
  :general
  (pspmacs/leader-keys
    :states 'normal
    :keymaps 'prog-mode-map
    "ls" '(:ignore t :wk "server (eglot)")
    "lss" '(eglot :wk "start"))

  (pspmacs/local-leader-keys
    :states 'normal
    :keymaps 'eglot-mode-map
    "g" '(:ignore t :wk "o to")
    "gg" '(xref-find-definitions :wk ":Symbol definition")
    "L" '(:ignore t :wk ":Eglot")
    "Lr" '(eglot-rename :wk "ename")
    "Ls" '(:ignore t :wk "erver")
    "Lsa" '(eglot-code-actions :wk "ct")
    "Lsr" '(eglot-reconnect :wk "econnect")
    "Lss" '(eglot-shutdown :wk "hutdown")
    "LsS" '(eglot-shutdown-all :wk "hutdown all"))
  :custom
  (eglot-extend-to-xref t)
  (eglot-connect-timeout 60)
  :hook
  (prog-mode . eglot-ensure)
  (eglot-managed-mode . pspmacs/eglot-capf))

(use-package flymake
  :defer t
  :general
  (pspmacs/leader-keys
    :states 'normal
    :keymaps 'flymake-mode-map
    "e" '(:ignore t :wk "rr")
    "en" '(flymake-goto-next-error :wk "ext")
    "ep" '(flymake-goto-prev-error :wk "revious"))
  :custom
  (flymake-suppress-zero-counters t)
  (flymake-number-of-errors-to-display nil)
  (python-flymake-command '("flake8" "-"))
  :hook
  ((eglot-managed-mode . flymake-mode)))

(use-package eldoc
  :general
  (pspmacs/local-leader-keys
    :state 'normal
    :keymaps 'prog-mode-map
    "d" '(:ignore t :wk "oc")
    "dd" '((lambda ()
             (interactive)
             (progn
               (display-local-help)
               (switch-to-buffer-other-window "*eldoc*")))
           :wk "escribe")))

(use-package display-fill-column-indicator
  :demand t
  :hook
  (prog-mode . display-fill-column-indicator-mode)
  :init
  (customize-set-variable 'fill-column 80))

(use-package color-fci
  :vc (color-fci :url "https://gitlab.com/pradyparanjpe/color-fci")
  :custom-face (color-fci-overflow ((default (:background "#ffffff"))))
  :hook (display-fill-column-indicator-mode . color-fci-mode))

(use-package semantic
  :demand t)
(mapc (lambda (x)
        "Add semantic mode, semantic-idle-breadcrumbs mode

to_list all modes that have parsers"
        (let ((target-hook
               (intern (format "%s-hook" (symbol-name (car x))))))
          (add-hook target-hook #'semantic-idle-breadcrumbs-mode)
          (add-hook target-hook #'semantic-mode)))
      semantic-new-buffer-setup-functions)

(add-hook 'prog-mode-hook #'pspmacs/after-code-load)

(use-package emacs
  :init
  ;; corfu suggestions
  ;; TAB cycle if there are only few candidates
  (setq completion-cycle-threshold 3)
  ;; Emacs 28: Hide commands in M-x which do not apply to the current mode.
  ;; Corfu commands are hidden, since they are not supposed to be used via M-x.
  (setq read-extended-command-predicate
        #'command-completion-default-include-p)
  ;; Enable indentation+completion using the TAB key.
  ;; `completion-at-point' is often bound to M-TAB.
  :custom
  (tab-always-indent 'complete)
  :general
  (pspmacs/leader-keys
    :states 'normal
    :keymaps 'prog-mode-map
    "C"  '(:ignore t :wk "ompile")
    "Cc" '((lambda ()
             (interactive)
             (progn
               (call-interactively 'compile)
               (switch-to-buffer-other-window "*compilation*")))
           :wk "ompile")
    "Ci" '((lambda ()
             (interactive)
             (progn
               (call-interactively 'pspmacs/project-init)))
           :wk "nit")
    "Cr" '((lambda ()
             (interactive)
             (progn
               (call-interactively 'pspmacs/serve-or-run)))
           :wk "un")))

(pspmacs/load-inherit)
;;; pspmacs-programming.el ends here

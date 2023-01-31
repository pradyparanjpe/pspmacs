(use-package doom-themes
  :custom
  ;; Global settings (defaults)
  ;; if nil, bold is universally disabled
  (doom-themes-enable-bold t)
  ; if nil, italics is universally disabled
  (doom-themes-enable-italic t)

  :config
  ;; Enable flashing mode-line on errors
  (doom-themes-visual-bell-config)

  ;; Corrects (and improves) org-mode's native fontification.
  (doom-themes-org-config)
  (load-theme 'doom-one t))

 (use-package space-theming
   :straight (:type git :host github :repo "p3r7/space-theming")
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
   (setq space-theming--current-theme 'doom-one)
   (space-theming-init-theming)
   (space-theming-update-current-theme))

(pspmacs/load-inherit)

;;; pspmacs-interface-enhancement.el ends here
(use-package modus-themes
  :straight t
  :demand t
  :general (pspmacs/leader-keys
             "T" '(:ignore t :wk "Theme")
             "Tt" 'modus-themes-toggle)
  :init
  (setq display-time-24hr-format t)
  (display-time-mode)
  (setq display-time-default-load-average nil)
  (line-number-mode t)
  (column-number-mode t)
  (size-indication-mode t)
  (defun pspmacs/modus-themes-custom-faces ()
    "Customize modus theme faces."
    (modus-themes-with-colors
      (progn
        (custom-set-faces
         ;; Add "padding" to the mode lines
         `(hl-line ((,c :slant italic)))
         `(org-document-title ((,c :foreground "#ffff9f")))
         `(font-function-name-face ((,c :foreground "#9f5f9f" :weight bold)))
         `(font-lock-comment-face ((,c :foreground "#3f4f5f" :background "#0f0f0f")))
         `(line-number ((,c :foreground "#4f5f7f" :background "#000000")))
         `(font-lock-type-face ((,c :foreground "#ff3f5f" :weight bold)))
         `(font-lock-rpack-face ((,c :foreground "#9f7fff")))
         `(font-lock-relem-face ((,c :foreground "#bf8faf")))
         `(font-lock-rsuper-face ((,c :foreground "#8fafbf")))
  ;;        `(mode-line ((,c :underline ,border-mode-line-active
  ;;                         :overline ,border-mode-line-active
  ;;                         :box (:line-width 10 :color ,bg-mode-line-active))))
  ;;        `(mode-line-inactive
  ;;          ((,c :underline ,border-mode-line-inactive
  ;;               :overline ,border-mode-line-inactive
  ;;               :box (:line-width 10 :color ,bg-mode-line-inactive))))
          ))))


  ;; ESSENTIAL to make the underline move to the bottom of the box:
  (setq x-underline-at-descent-line t)
  :custom
  ;; Add all your customizations prior to loading the themes
  (modus-themes-org-blocks 'gray-background)
  (modus-themes-custom-auto-reload t)
  (modus-themes-italic-constructs t)
  (modus-themes-bold-constructs t)
  (modus-themes-mixed-fonts t)
  (modus-themes-variable-pitch-ui nil)

  ;; Maybe define some palette overrides, such as by using our presets
  (modus-vivendi-palette-overrides
   '((bg-main   "#000307")
     (fg-main   "#959a9f")
     (fg-heading-1 "#5f6fbf")
     (fg-heading-2 "#afaf5f")
     (fg-heading-3 "#5faf5f")
     (fg-heading-4 "#af5faf")
     (fg-heading-5 "#5fbfbf")
     (fg-heading-6 "#af5f5f")
     (fg-heading-7 "#5f5f5f")
     (fg-heading-8 "#afafaf")
     ;; (bg-mode-line-active bg-main)
     ;; (fg-mode-line-active fg-main)
     (modus-themes-disable-other-themes t)
     ;; (border-mode-line-active bg-mode-line-active)
     ;; (border-mode-line-inactive bg-mode-line-inactive)
     (constant  "#af1f1f")
     (builtin   "#d76f10")
     (docstring "#875f3f")
     (keyword   "#00afff")
     (string    "#1faf5f")
     (variable  "#ffffaf")))
  ;; Load the theme of your choice.
  ;; (load-theme 'modus-operandi :no-confirm)
  :hook
  (modus-themes-after-load-theme . pspmacs/modus-themes-custom-faces))

(load-theme 'modus-vivendi :no-confirm)
;; Somehow, the hook isn't getting evaluated automatically
(eval modus-themes-after-load-theme-hook)

(pspmacs/load-inherit)
;;; pspmacs-interface-enhancement.el ends here

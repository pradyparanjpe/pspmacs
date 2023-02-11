(pspmacs/install-git-clone
 '(modus-themes :repo "protesilaos/modus-themes"))

(use-package modus-themes
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

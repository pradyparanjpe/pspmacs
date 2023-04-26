(when (version< emacs-version "29")
  (pspmacs/install-git-clone
   '(modus-themes :repo "protesilaos/modus-themes")
   :force t))

(setq modus-kw
      (if (version< emacs-version "29")
          (if (string= pspmacs/package-manager "straight")
                                        ; use straight.
              '(:straight t)
                                        ; use crooked.
            `(:load-path ,(expand-file-name
                           "modus-themes" pspmacs/crooked-dir)))
                                        ; use builtin use-vc-package.
        '(:vc (modus-themes
               . (:url "https://www.github.com/protesilaos/modus-themes")))))

(setq modus-kwargs
      `(modus-themes
        :ensure nil
        :demand t
        ,@modus-kw
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
           (builtin   "#d76f10")
           (constant  "#af1f1f")
           (variable  "#ffffaf")
           (keyword   "#00afff")
           (string    "#1faf5f")
           (fg-heading-1 "#5f6fbf")
           (fg-heading-2 "#afaf5f")
           (fg-heading-3 "#5faf5f")
           (fg-heading-4 "#af5faf")
           (fg-heading-5 "#5fbfbf")
           (fg-heading-6 "#af5f5f")
           (fg-heading-7 "#5f5f5f")
           (fg-heading-8 "#afafaf")
           (bg-mode-line-active bg-main)
           (fg-mode-line-active fg-main)
           (modus-themes-disable-other-themes t)
           (border-mode-line-active bg-mode-line-active)
           (border-mode-line-inactive bg-mode-line-inactive)))
        ;; Load the theme of your choice.
        ;; (load-theme 'modus-operandi :no-confirm)
        :hook
        (modus-themes-after-load-theme . pspmacs/modus-themes-custom-faces)))

(eval `(use-package ,@modus-kwargs))

(use-package emacs
  :custom
  (custom-theme-directory xdg/emacs-cache-directory))

(pspmacs/load-inherit)
;;; pspmacs-interface-enhancement.el ends here

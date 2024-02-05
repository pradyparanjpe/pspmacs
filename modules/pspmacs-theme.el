;;; pspmacs-theme.el --- Emacs themes -*- lexical-binding: t; -*-

;; Copyright © 2023  Pradyumna Swanand Paranjape

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

(use-package modus-themes
  :demand t
  :after fontaine
  :vc (modus-themes :url "https://www.github.com/protesilaos/modus-themes")
  :general (pspmacs/leader-keys
             "t" '(:ignore t :wk "Theme")
             "tT" 'modus-themes-toggle)
  :init
  ;; ESSENTIAL to make the underline move to the bottom of the box:
  (setq x-underline-at-descent-line t)

  :custom
  ;; Add all your customizations prior to loading the themes
  (modus-themes-org-blocks 'gray-background)
  (modus-themes-custom-auto-reload t)
  (modus-themes-italic-constructs t)
  (modus-themes-bold-constructs t)
  (modus-themes-mixed-fonts t)
  (modus-themes-variable-pitch-ui t)

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
     (bg-mode-line-active "#000000")
     (fg-mode-line-active "#7f7f7f")
     (modus-themes-disable-other-themes t)
     (border-mode-line-active bg-mode-line-active)
     (border-mode-line-inactive bg-mode-line-inactive)))
  ;; Load the theme of your choice.
  ;; (load-theme 'modus-operandi :no-confirm)
  :hook
  (pspmacs/after-enable-theme . pspmacs/modus-themes-custom-faces))

(use-package fontaine
  :demand t
  :general
  (pspmacs/leader-keys
    "Ff" '(fontaine-set-preset :wk "set")
    "FF" '(fontaine-set-face-font :wk "face")
    "F=" '(text-scale-adjust :wk "🔍")
    "F-" '(text-scale-adjust :wk "🔍")
    "F0" '(text-scale-adjust :wk "🔍"))
  :custom
  (fontaine-latest-state-file
   (xdg/make-path "fontaine-latest-state.eld" 'state))
  (fontaine-presets
   `((tiny
      :default-height ,(round (* pspmacs/font-height 0.7)))
     (small
      :default-height ,(round (* pspmacs/font-height 0.9)))
     (regular
      :default-height ,(round (* pspmacs/font-height 1)))
     (medium
      :default-height ,(round (* pspmacs/font-height 1.1)))
     (large
      :default-weight light
      :default-height ,(round (* pspmacs/font-height 1.4))
      :bold-weight bold)
     (presentation
      :default-weight light
      :default-height ,(round (* pspmacs/font-height 1.7))
      :bold-weight bold)
     (jumbo
      :default-weight light
      :default-height ,(round (* pspmacs/font-height 2.2))
      :bold-weight bold)
     (t
      :default-family "Fira Code Nerd Font"
      :default-weight regular
      :default-height ,pspmacs/font-height
      :fixed-pitch-family "Fira Code Nerd Font"
      :fixed-pitch-weight regular
      :fixed-pitch-height 1.0
      :fixed-pitch-serif-family nil
      :fixed-pitch-serif-weight nil
      :fixed-pitch-serif-height 1.0
      :variable-pitch-family "Cantarell Nerd Font"
      :variable-pitch-weight regular
      :variable-pitch-height 1.0
      :bold-family nil
      :bold-weight bold
      :italic-family "VictorMono"
      :italic-slant italic
      :line-spacing nil)))
  :hook
  (kill-emacs-hook . fontaine-store-latest-preset)
  (pspmacs/after-enable-theme . fontaine-apply-current-preset)
  :config
  (if (daemonp)
      (add-hook 'after-make-frame-functions
                (lambda (frame)
                  (with-selected-frame frame
                    (fontaine-set-preset (or (fontaine-restore-latest-preset) 'regular)))))
    (fontaine-set-preset (or (fontaine-restore-latest-preset) 'regular))))

(use-package emacs
  :custom
  (custom-theme-directory xdg/emacs-cache-directory)
  :hook
  (text-mode . variable-pitch-mode))

(pspmacs/load-inherit)
;;; pspmacs-theme.el ends here

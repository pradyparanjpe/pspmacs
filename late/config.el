;;; late/config.el --- Final configurations -*- lexical-binding: t; -*-

(load-theme 'modus-vivendi :no-confirm)
;; Somehow, the hook isn't getting evaluated automatically
(eval modus-themes-after-load-theme-hook)

(pspmacs/load-inherit)

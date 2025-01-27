;;; internet.el --- internet ide -*- lexical-binding: t; -*-

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

;; (declare-function bookmark-make-record-default "bookmark" (&optional no-file no-context posn))
;; (declare-function bookmark-prop-get "bookmark" (bookmark prop))
;; (declare-function bookmark-default-handler "bookmark" (bmk))
;; (declare-function bookmark-get-bookmark-record "bookmark" (bmk))

(use-package eww
  :general
  (pspmacs/leader-keys
    :states 'normal
    "ib" '(eww-switch-to-buffer :wk "uffers")
    "io" '(eww t :wk "pen")
    "is" '(:ignore t :wk "earch")
    "isw" '(eww-search-words :wk "ords"))

  :init
  (defun karthink/reader-center-images ()
    "Center images in document. Meant to be added to a post-render hook."
    (let* ((inhibit-read-only t)
           (pixel-buffer-width (shr-pixel-buffer-width))
           match)
      (save-excursion
        (goto-char (point-min))
        (while (setq match (text-property-search-forward
                            'display nil
                            (lambda (_ p) (eq (car-safe p) 'image))))
          (when-let ((size (car (image-size
                                 (prop-match-value match) 'pixels)))
                     ((> size 150))
                     (center-pixel (floor (- pixel-buffer-width size) 2))
                     (center-pos (floor center-pixel (frame-char-width))))
            (beginning-of-line)
            (delete-horizontal-space)
            (indent-to center-pos)
            (end-of-line))))))

  :custom
  (eww-auto-rename-buffer t)
  (eww-search-prefix "https://duckduckgo.com/html/?q=")
  (eww-bookmarks-directory xdg/emacs-cache-directory)
  (eww-browse-url-new-window-is-tab nil)
  (shr-width fill-column)
  (url-configuration-directory
   (file-name-as-directory (xdg/make-path "url" 'cache)))
  (url-cookie-file (expand-file-name "cookies" url-configuration-directory))

  :hook
  ((eww-after-render . (lambda () (setq line-spacing 0.1)))
   (eww-after-render . karthink/reader-center-images)))

(use-package gptel
  :commands (gptel gptel-menu)
  :general
  (pspmacs/leader-keys
    "A"     '(:ignore t  :wk "I")
    "A g"   '(:ignore t  :wk "pt")
    "A g l" '(gptel      :wk "aunch")
    "A g m" '(gptel-menu :wk "enu"))
  :custom (gptel-default-mode 'org-mode)
  :hook (gptel-post-stream . gptel-auto-scroll)
  :config (add-hook 'gptel-post-response-functions 'gptel-end-of-response))

(use-package mu4e
  :when pspmacs/mu4e-load-path
  :ensure nil
  :load-path pspmacs/mu4e-load-path
  :defer 20
  :after (evil-collection org-mime)
  :general
  (pspmacs/leader-keys
    "<"  '(:ignore t :wk "mail")
    "<<" '(mu4e :wk "main")
    "<u" '(mu4e-update-mail-and-index :wk "pdate")
    "<c" '(mu4e-compose-new :wk "ompose"))

  (pspmacs/leader-keys :keymaps 'mu4e-compose-mode-map
    "oe" '(org-mime-edit-mail-in-org-mode :wk "dit")
    "<h" '(:ignore t :wk "tmlize")
    "<-" '(org-mime-revert-to-plain-text-mail :wk "revert"))

  (pspmacs/leader-keys
    :keymaps '(mu4e-compose-mode-map)
    ;; NEXT: add as send-mail hook
    "<h"  '(:ignore t :wk "tmlize")
    "<hh" '(org-mime-htmlize :wk "this"))

  :custom
  (mu4e-notification-support t)
  (mu4e-modeline-support t)
  (mu4e-compose-format-flowed t)
  (mu4e-get-mail-command (format
                          "mbsync -c %s -a"
                          (expand-file-name
                           "mu4e/mbsyncrc"
                           (or (getenv "XDG_CONFIG_HOME") "~/.config"))))
  (mu4e-change-filenames-when-moving t)
  (mu4e-update-interval (* 1 60 60))

  :hook
  (mu4e-compose-mode . display-fill-column-indicator-mode)

  :config
  (mu4e t)
  (evil-collection-mu4e-setup))

(use-package mu4e-org
  :when pspmacs/mu4e-load-path
  :ensure nil
  :load-path pspmacs/mu4e-load-path
  :after (mu4e org-capture)
  :init
  ;; Ensure File exists
  (make-directory (file-name-directory pspmacs/org-mail-path) t)
  (unless (file-exists-p pspmacs/org-mail-path)
    (write-region "\n* Follow up\n\n* Read later" nil pspmacs/org-mail-path))
  (pspmacs/extend-list
   'org-capture-templates
   `(("m" "Mail")
     ("mf" "Follow up" entry (file+olp ,pspmacs/org-mail-path "Follow up")
      ,(string-join
        '("* About %a"
          "** With %:fromname"
          "** Created: %:date-timestamp-inactive"
          "")
        "\n"))
     ("mr" "Read later" entry (file+olp ,pspmacs/org-mail-path "Read Later")
      ,(string-join
        '("* About %a"
          "** From %:fromname"
          "** Created: %:date-timestamp-inactive"
          "")
       "\n")))))

(use-package gnus
  :custom
  (gnus-select-method '(nnnil nil))
  (gnus-home-directory (xdg/make-path "gnus" 'cache :directory))
  (gnus-directory (xdg/make-path "gnus/News" 'cache :directory))
  (gnus-cache-directory (xdg/make-path "gnus/News/cache" 'cache :directory))
  (gnus-kill-files-directory (xdg/make-path "gnus/News" 'cache :directory))
  (gnus-article-save-directory (xdg/make-path "gnus/News" 'cache :directory))
  (gnus-init-file (xdg/make-path "gnus/.gnus" 'cache))
  (gnus-startup-file (xdg/make-path "gnus/.newsrc" 'cache))
  :config
  (add-to-list 'recentf-exclude (format "%s.*\\'" gnus-directory)))

(use-package smtpmail
  :ensure nil
  :after message
  :custom
  (message-send-mail-function #'smtpmail-send-it)
  (send-mail-function #'smtpmail-send-it)
  (message-send-mail-function #'smtpmail-send-it)
  :hook
  (message-send . mml-secure-message-sign-pgpmime))

(use-package org-mime
  :commands (org-mime-confirm-when-no-multipart
             org-mime-edit-mail-in-org-mode)
  :defer 20
  :general
  (pspmacs/leader-keys
    :keymaps '(org-mode-map)
    "<h"  '(:ignore t :wk "tmlize")
    "<hb" '(org-mime-org-buffer-htmlize :wk "uffer")
    "<hs" '(org-mime-org-subtree-htmlize :wk "ubtree"))

  (pspmacs/leader-keys
    :keymaps '(org-mode-map)
    ;; NEXT: add as send-mail hook
    "<h"  '(:ignore t :wk "tmlize")
    "<hh" '(org-mime-htmlize :wk "this"))

  :custom
  (org-mime-export-options
   '(:section-numbers nil :with-author nil :with-toc nil))
  (org-mime-library 'mml)

  :hook
  (message-send . org-mime-confirm-when-no-multipart)
  (org-mime-html . (lambda ()
                     (org-mime-change-element-style
                      "pre"
                      (string-join
                       '("color: #959a9f"
                         "background-color: #000307"
                         "padding: 0.5em;")
                       "; ")))))

(use-package emacs
  :custom
  (mail-source-directory
   (expand-file-name
    "Maildir" (or (getenv "XDG_DATA_HOME")
                  (expand-file-name ".local/share" (getenv "HOME")))))
  (mail-default-directory (expand-file-name
    "Maildir/drafts" (or (getenv "XDG_DATA_HOME")
                         (expand-file-name ".local/share" (getenv "HOME")))))
  (message-auto-save-directory (expand-file-name
    "Maildir/drafts" (or (getenv "XDG_DATA_HOME")
                         (expand-file-name ".local/share" (getenv "HOME")))))
  (message-signature (format "\n\n-- \n%s" user-full-name))
  (browse-url-generic-program (or (executable-find "qutebrowser")
                                  (executable-find "firefox")
                                  (executable-find "chromium-freeworld")
                                  (executable-find "google-chrome")))
  (browse-url-browser-function 'browse-url-generic))

(pspmacs/load-inherit)
;;; pspmacs-internet.el ends here

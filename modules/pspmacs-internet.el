;;; internet.el --- internet ide -*- lexical-binding: t; -*-

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

;; (declare-function bookmark-make-record-default "bookmark" (&optional no-file no-context posn))
;; (declare-function bookmark-prop-get "bookmark" (bookmark prop))
;; (declare-function bookmark-default-handler "bookmark" (bmk))
;; (declare-function bookmark-get-bookmark-record "bookmark" (bmk))

(use-package eww
  :general
  (pspmacs/leader-keys
    :states 'normal
    "io" '(eww t :wk "open")
    "is" '(:ignore t :wk "search")
    "isw" '(eww-search-words :wk "words"))
  (pspmacs/leader-keys
    :states 'normal
    :keymaps 'eww-mode-map
    "iu" '(:ignore t :wk "url")
    "iuy" '(eww-copy-page-url t :wk "copy"))

  :init
  (defun karthink/reader-center-images ()
    "Center images in document. Meant to be added to a post-render
 hook."
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
  (eww-search-prefix "https://duckduckgo.com/html/?q=")
  (eww-browse-url-new-window-is-tab nil)
  (shr-width fill-column)

  :hook
  ((eww-after-render . (lambda () (setq line-spacing 0.1)))
   (eww-after-render . karthink/reader-center-images)))

(use-package org-mime
  :custom
  (org-mime-library 'mml))

(when pspmacs/set-mailbox
  (use-package mu4e
    :init
    (setq message-send-mail-function 'smtpmail-send-it)
    :ensure nil
    :custom
    (mu4e-account-alist t)
    (mu4e-enable-notifications t)
    (mu4e-enable-mode-lineu4e-enable-mode-line t)
    (mu4e-compose-signature-auto-include t)
    (mu4e-compose-signature (format "%s\n%s" "--" user-full-name))
    (mu4e-compose-format-flowed t)
    (mu4e-get-mail-command (format
                            "mbsync -c %s -a"
                            (expand-file-name
                             "mu4e/mbsyncrc"
                             (or (getenv "XDG_CONFIG_HOME") "~/.config"))))
    (mu4e-maildir (expand-file-name
                   "Maildir" (or (getenv "XDG_DATA_HOME") "~/.local/share")))
    (mu4e-change-filenames-when-moving t)
    (mu4e-update-interval (* 10 60 60))
    (mu4e-view-show-images t)
    (mu4e-view-show-addresses t)
    :hook
    ((message-send . mml-secure-message-sign-pgpmime)
     (message-send . org-mime-confirm-when-no-multipart)
     (org-mime-html . (lambda ()
                        (org-mime-change-element-style
                         "pre"
                         (format
                          "color: %s; background-color: %s; padding: 0.5em;"
                          "#959a9f" "#000307")))))))

(use-package emacs
  :custom
  (browse-url-generic-program (or (executable-find "qutebrowser")
                                  (executable-find "firefox")
                                  (executable-find "chromium-freeworld")
                                  (executable-find "google-chrome")))
  (browse-url-browser-function 'browse-url-generic))

(pspmacs/load-inherit)
;;; pspmacs-internet.el ends here

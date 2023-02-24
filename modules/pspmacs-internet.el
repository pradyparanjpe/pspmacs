;;; internet.el --- internet ide -*- lexical-binding: t; -*-

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

(use-package org-mime
  :custom
  (org-mime-library 'mml))

(when pspmacs/set-mailbox
  (use-package mu4e
    :init
    (setq message-send-mail-function 'smtpmail-send-it)
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

(pspmacs/load-inherit)
;;; pspmacs-internet.el ends here

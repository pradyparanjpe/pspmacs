;;; bootstrap-builtin.el --- package.el package manager -*- lexical-binding: t; -*-

;; package configuration
;; Code:
(require 'time-date)
(require 'package)
(defcustom pspmacs/archives-stale-days 1
  "Days after which archives become stale."
  :type 'integer)

(defun pspmacs/archive-refreshed-recently-p (archive)
  "If package ARCHIVE was initialized today.

t only if ARCHIVE's time-stamp within last `pspmacs/archives-stale-days'"
  (let* ((today (decode-time nil nil t))
         (archive-path (expand-file-name
                        (format "archives/%s/archive-contents" archive)
                        package-user-dir))
         (last-update-time (decode-time (file-attribute-modification-time
                                         (file-attributes archive-path))))
         (delta (make-decoded-time :day pspmacs/archives-stale-days)))
    (time-less-p (encode-time today)
                 (encode-time (decoded-time-add last-update-time delta)))))

(defun pspmacs/archives-refreshed-recently-p ()
  "All archives have been refreshed today."
  (interactive)
  (cl-every #'pspmacs/archive-refreshed-recently-p
            (mapcar #'car package-archives)))

(defun pspmacs/init-package-manager ()
  "Initialize `package.el' as the package manager"
  ;; Additional package archives
  (add-to-list 'package-archives '("stable" . "https://stable.melpa.org/packages/"))
  (add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))

  (customize-set-variable 'package-archive-priorities
                          '(("gnu"    . 99)
                            ("nongnu" . 80)
                            ("stable" . 70)
                            ("melpa"  . 0)))

  ;; package should store data locally.
  (customize-set-variable 'package-user-dir
                          (expand-file-name "packages" local-emacs-directory))
  (unless (file-exists-p package-user-dir)
    (mkdir package-user-dir t))
  (package-initialize)
  (unless (pspmacs/archives-refreshed-recently-p)
    (message "Refreshing package archives")
    (package-refresh-contents)))

;;; bootstrap-builtin.el ends here

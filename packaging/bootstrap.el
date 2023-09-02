;;; bootstrap.el --- bootstrap package manager -*- lexical-binding: t; -*-

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
    (cond ((not (file-readable-p archive-path)) nil)
          ((time-less-p (encode-time
                         (decoded-time-add last-update-time delta))
                        (encode-time today))
           nil)
          (t t))))

(defun pspmacs/archives-refreshed-recently-p ()
  "All archives have been refreshed today."
  (interactive)
  (cl-every #'pspmacs/archive-refreshed-recently-p
            (mapcar #'car package-archives)))

(defun pspmacs/init-use-package ()
  "Emacs v29 can install packages from version control.

vc-use-package is included in Emacs 30"
  (eval-and-compile
    (customize-set-variable 'use-package-compute-statistics t)
    (customize-set-variable 'use-package-always-ensure t)
    (customize-set-variable 'use-package-always-defer t)
    (customize-set-variable 'use-package-expand-minimally t))

  ;; NEXT Drop once released with GNU/Emacs (29 or 30)
  (when (version< emacs-version "30")
    (unless (package-installed-p 'vc-use-package)
      (package-vc-install "https://github.com/slotThe/vc-use-package"))
    (require 'vc-use-package)))

(defun pspmacs/init-package-manager ()
  "Initialize /=package.el/= as the package manager"
  ;; package should store data locally.
  (customize-set-variable 'package-user-dir
                          (expand-file-name "packages" local-emacs-dir))
  (unless (file-exists-p package-user-dir) (mkdir package-user-dir t))
  ;; Paranoia
  ;; (add-to-list 'package-archives
  ;;              '("stable" . "https://stable.melpa.org/packages/"))
  ;; ("stable" . 70)

  ;; Additional package archives
  (add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
  (customize-set-variable 'package-archive-priorities
                          '(("gnu"    . 99)
                            ("nongnu" . 80)
                            ("melpa"  . 0)))

  (package-initialize)
  (unless (pspmacs/archives-refreshed-recently-p)
    (message "Refreshing package archives…")
    (package-refresh-contents))
  (pspmacs/init-use-package))

(use-package auto-package-update
  :custom
  (auto-package-update-delete-old-versions t)
  (auto-package-update-hide-results t)
  (auto-package-update-prompt-before-update t)
  (auto-package-update-last-update-day-filename (expand-file-name
                                                 ".last-package-update-day"
                                                 xdg/emacs-state-directory))
  (auto-package-update-show-preview t)
  :config
  (auto-package-update-maybe))

(pspmacs/load-inherit)
;;; bootstrap.el ends here

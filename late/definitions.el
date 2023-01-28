;;; late/definitions.el --- identifies platform -*- lexical-binding: t; -*-
(require 'yaml)
(require 'ht)

(defconst ON-LINUX   (eq system-type 'gnu/linux))
(defconst ON-MAC     (eq system-type 'darwin))
(defconst ON-BSD     (or ON-MAC (eq system-type 'berkeley-unix)))
(defconst ON-WINDOWS (memq system-type '(cygwin windows-nt ms-dos)))

(load (expand-file-name "late/xdg.el" user-emacs-directory) nil 'nomessage)

(defvar pspmacs/modules-order-list
  (let
      ((modules-dir
        (mapcar
         (lambda (x)
           (expand-file-name "modules" x))
         `(,user-emacs-directory
           ,pvt-emacs-directory
           ,local-emacs-directory))))
       (apply
         'vconcat (mapcar
                   (lambda (x) (cdr x))
                   (sort
                    (ht->alist
                     (apply
                      'ht-merge
                      (remq 'nil
                            (mapcar
                             (lambda (x)
                               (let
                                   ((order-file (expand-file-name "load-order.yml" x)))
                                 (if (file-readable-p order-file)
                                     (yaml-parse-string (f-read-text order-file)))))
                             modules-dir))))
                    (lambda (a b) (< (car a) (car b)))))))
"Ordered list of pspmacs/modules to load")

(pspmacs/load-inherit)

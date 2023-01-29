;;; late/init.el --- Late init file, loads modules -*- lexical-binding: t; -*-

(when (featurep 'chemacs)
  (customize-set-variable 'package-user-dir
                          (expand-file-name "elpa" local-emacs-directory)))

(load (expand-file-name "bootstrap-package.el" pspmacs/packaging-directory)
      nil 'nomessage)

(dolist (init-dir
         `(,user-emacs-directory
           ,pvt-emacs-directory
           ,local-emacs-directory) nil)
  (let ((modular-modules (expand-file-name "modules/" init-dir)))
    (when (file-directory-p modular-modules)
      (setq load-path
            (append (let ((load-path (list))
                          (default-directory modular-modules))
                      (add-to-list 'load-path modular-modules)
                      ;;(normal-top-level-add-to-load-path '("."))
                      (normal-top-level-add-subdirs-to-load-path)
                      load-path)
                    load-path)))))

(customize-set-variable 'custom-file
  (expand-file-name "custom.el" local-emacs-directory))

(setq gc-cons-threshold (* 2 1000 1000))

(use-package yaml)
(use-package ht)
(use-package f)

(defvar pspmacs/modules-order
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
                                   ((order-file
                                     (expand-file-name "load-order.yml" x)))
                                 (if (file-readable-p order-file)
                                     (yaml-parse-string
                                      (f-read-text order-file)))))
                             modules-dir))))
                    (lambda (a b) (< (car a) (car b)))))))
"Ordered list of pspmacs/modules to load")

(load (expand-file-name "late/org-latest.el" user-emacs-directory))

(seq-doseq (autofile pspmacs/modules-order nil)
  (dolist
      (work-tree
       `(,user-emacs-directory ,pvt-emacs-directory ,local-emacs-directory)
       nil)
    (catch 'load-success
      (let* ((lit-module
              (expand-file-name
               (format "modules/pspmacs-%s.org" autofile) work-tree))
             (found (when (file-readable-p lit-module)
                      (pspmacs/load-suitable lit-module)
                      lit-module)))
        (when found (throw 'load-success lit-module))))))

(pspmacs/load-inherit)

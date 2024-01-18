;;; late/definitions.el --- define values -*- lexical-binding: t; -*-

(load (expand-file-name "late/version.el" user-emacs-directory) nil 'nomessage)

(defgroup pspmacs nil
  "PSPMacs Custom init configuration for Emacs"
  :version pspmacs/version)

(defun pspmacs/load-suitable (fname &optional nag)
  "Load Emacs init file FNAME.

Function defined in early/definitions.el is hereby redefined to enable
`org-babel-load-file' method, now that the correct `org-mode' is loaded.

If FNAME is found, load it and return.
If org/el counterpart of FNAME is found, load it and return.
To load,

If extension `string='s 'org', use function `org-babel-load-file'.
If extension `string='s 'el', use function `load'

If nothing is found and if NAG is t, throw error. Default: return"
  (cond
   ((string= (file-name-extension fname) "org")
    (cond ((file-readable-p fname)
           (org-babel-load-file fname))
          ((file-readable-p (file-name-with-extension fname "el"))
           (load (file-name-with-extension fname "el") nil 'nomessage))))
   ((string= (file-name-extension fname) "el")
    (cond ((file-readable-p fname)
           (load fname nil 'nomessage))
          ((file-readable-p (file-name-with-extension fname "org"))
           (org-babel-load-file (file-name-with-extension fname "org")))))
   (nag (user-error (format "Neither %s.{el,org} found."
                            (file-name-sans-extension fname))))))

(add-to-list 'load-path (expand-file-name "pspack" user-emacs-directory))
(require 'pspmacs/xdg)
(require 'pspack)

(pspmacs/load-inherit)

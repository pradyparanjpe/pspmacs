;;; pspack/pspack.el --- common pspmacs utils -*- lexical-binding: t; -*-
;;; Commentary:
;;
;; Custom variables, functions, faces for pspmacs config.
;;
;;; Code:
(defgroup pspack nil
  "Functions and variables used by pspmacs, bundled into a package."
  :group 'pspack)

(load (expand-file-name "vars.el" (file-name-directory load-file-name))
      nil 'nomessage)
(load (expand-file-name "func.el" (file-name-directory load-file-name))
      nil 'nomessage)
(provide 'pspack)

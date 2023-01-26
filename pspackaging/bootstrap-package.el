;;; bootstrap-package.el --- packaging system -*- lexical-binding: t; -*-
(load
  (expand-file-name "bootstrap-straight.el" pspmacs/packaging-directory)
  nil 'nomessage)

(eval-and-compile (setq use-package-expand-minimally t))

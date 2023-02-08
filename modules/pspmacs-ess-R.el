(use-package ess
  :mode ("\\.R\\'" . R-mode)
  :defer t
  :hook
  ((R-mode . pspmacs/prettify-R)
   (R-mode . pspmacs/rfaces)))

(use-package ess
  :defer t
  :init
  (defun pspmacs/rfaces ()
    "R callable hook function"
    (font-lock-add-keywords
     nil
     '(("\\W\\(\\(\\s_\\|\\sw\\|\\.\\)+\\)::"
        1 '(:foreground "#9f7fff") t)))
    (font-lock-add-keywords
     nil
     '(("\\w::\\(\\(\\s_\\|\\sw\\|\\.\\)+\\)"
        1 '(:foreground "#8fa7bf") t)))
    (font-lock-add-keywords
     nil
     '(("\\(\\(\\s_\\|\\sw\\|\\.\\)+\\)\\$\\w"
        1 '(:foreground "#bf8faf") t)))
    (font-lock-add-keywords
     nil
     '(("\\w\\$\\(\\(\\s_\\|\\sw\\|\\.\\)+\\)"
        1 '(:foreground "#8fa7bf") t))))
  :config
  (pspmacs/rfaces)
  (pspmacs/mode-prettify '("code" "R")))

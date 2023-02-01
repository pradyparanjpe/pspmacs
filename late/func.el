(defun pspmacs/inferior-interpreter (executable)
  "Open an inferior interpreter in split window

Open EXECUTABLE interpreter in an inferior windows."
  (interactive)
  (let ((interpreter-window (split-window-below)))
    (select-window interpreter-window)
    (call-interactively executable)))

(defun pspmacs/destroy-buffer-and-window (&optional target-buffer)
  "Destroy window and buffer after some process is done

If TARGET-BUFFER is supplied, it and its window is destroyed.
Else, current buffer and window is destroyed.
If window is the only window, it is spared"
  (let* ((used-buffer (or target-buffer (current-buffer)))
         (used-window (get-buffer-window used-buffer)))
    (when (not (one-window-p))
      (delete-window used-window))
    (kill-buffer used-buffer)))

(defun pspmacs/kill-other-buffers ()
  "Kill all other buffers."
  (interactive)
  (mapc 'kill-buffer (delq (current-buffer) (buffer-list))))

(defun pspmacs/extend-list (list-var elements)
  "Iterative form of ‘add-to-list’.

Return value is the new value of LIST-VAR"
  (unless (consp elements)
    (error "ELEMENTS must be list"))
  (dolist (elem elements)
    (add-to-list list-var elem))
  (symbol-value list-var))

(defun pspmacs/mode-prettify (sub-modes)
  "Apply pretiffy mode alist according to active-mode"
  (progn
    (setq
     prettify-symbols-alist
     (mapcan (lambda (x)
               (list x `(,(upcase (car x)) . ,(cdr x))))
             (apply #'append
                    (mapcar
                     (lambda (y)
                       (cdr (assoc y pspmacs/pretty-alist)))
                     sub-modes))))
    (prettify-symbols-mode)))

(defun pspmacs/load-suitable (fname &optional nag)
  "Load emacs init file FNAME.

Function defined in early/definitions.el is hereby redefined to enable
`org-babel-load-file' method, now that the correct org-mode is loaded.

If FNAME is found, load it and return.
If org/el counterpart of FNAME is found, load it and return.
To load,

If extension `string='s 'org', use function `org-babel-load-file'.
If extension `string='s 'el', use function `load'

If nothing is found and if NAG is `t', throw error. Default: return"
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

(pspmacs/load-inherit)

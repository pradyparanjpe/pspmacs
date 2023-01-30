;;; late/definitions.el --- identifies platform -*- lexical-binding: t; -*-

(defconst ON-LINUX   (eq system-type 'gnu/linux))
(defconst ON-MAC     (eq system-type 'darwin))
(defconst ON-BSD     (or ON-MAC (eq system-type 'berkeley-unix)))
(defconst ON-WINDOWS (memq system-type '(cygwin windows-nt ms-dos)))

(defun pspmacs/inferior-interpreter (executable)
  "Open an inferior interpreter in split window

Open EXECUTABLE interpreter in an inferior window
and kill buffer and destroy window when EXECUTABLE
process exits"
  (interactive)
  (message (format "executing: %s" executable))
  (let ((interpreter-window (split-window-below)))
    (select-window interpreter-window)
    (call-interactively executable)))

(load (expand-file-name "late/xdg.el" user-emacs-directory) nil 'nomessage)

(pspmacs/load-inherit)

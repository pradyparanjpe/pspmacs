;;; pspline.el --- pspline modeline -*- lexical-binding: t; -*-
;;; Commentary:
;;
;; PSPLINE file-system awareness for pspmacs
;;
;;; Code:
(defgroup pspline nil
  "PSPLINE awareness for pspmacs."
  :group 'pspmacs)

(use-package all-the-icons
  :if (display-graphic-p))

(defun pspmacs/pspline-buffer-focused-p ()
  "Is the `current-buffer' focused"
  (eq (current-buffer) (window-buffer (selected-window))))

(defvar pspmacs/pspline-evil-state
  '(let ((fg-col (pcase evil-state ('normal "#ff9f00")
                        ('insert "#00ff9f")
                        ('visual "#009fff")
                        ('replace "#ffff3f")
                        ('operator "#ff009f")
                        ('motion "#3fffff")
                        ('emacs "#bfbfbf")
                        (_ "#000000"))))
     (propertize "● "
                 'face (if (pspmacs/pspline-buffer-focused-p)
                           `(:foreground ,fg-col :height 1.5)
                         mode-line-inactive)
                 'help-echo (symbol-name evil-state)))

  "Evil state dot")

(defvar pspmacs/pspline-buffer-state
  '(propertize "%3p/%5I "
               'face (if (pspmacs/pspline-buffer-focused-p)
                         '(:foreground "#5fafff")
                       'mode-line-inactive))

  "Buffer-state")

(defvar pspmacs/pspline-major-icon
  '(when (display-graphic-p)
     (concat
      (propertize
       (let*
           ((icon (ignore-errors
                    (all-the-icons-icon-for-buffer)))
            (icon (if icon
                      icon
                    (all-the-icons-icon-for-mode major-mode))))
         icon)
       'help-echo (symbol-name major-mode))
      " "))

  "Major mode icon")

(defvar pspmacs/pspline-buffer-name
  '(let* ((base (if (buffer-modified-p)
                    '(:foreground "#ff8f9f")
                  (if (pspmacs/pspline-buffer-focused-p)
                      'mode-line-buffer-id
                    'mode-line-inactive)))
          (box (if buffer-read-only '(:box t) '(:box nil))))
     (concat
      (propertize
       (or
        (ignore-errors
          (file-relative-name buffer-file-name (projectile-project-mode)))
        "%b")
       'face `(,base ,box))
      (if mode-line-process (propertize (format " [%s]" mode-line-process)
                                        'face '(:foreground modeline-info)))
      " "))

  "Buffer-name, process-state")

(defvar pspmacs/pspline-cursor-position
  '(concat
    (propertize "%02l:%02c "
                'face (if (pspmacs/pspline-buffer-focused-p)
                          '(:foreground "#afff5f")
                        'mode-line-inactive)))

  "Cursor: row:col")

(defvar pspmacs/pspline-version-control
  '(when (stringp vc-mode)
     (let ((vc-spec
            (replace-regexp-in-string
             (format "^ %s[:-]" (vc-backend buffer-file-name))
             " " vc-mode)))
       (propertize vc-spec
                   'face
                   `(:foreground ,(if (member vc-spec '(" main" " master"))
                                      "#3fff7f"
                                    "#7f3fff")))))
  "version control spec")

(defvar pspmacs/pspline-time
  '(propertize (format-time-string "⏲%H:%M")
               'face 'bold 'help-echo (format-time-string "%c"))

  "Time segment")

(setq-default
 mode-line-format
 `("%e"
   mode-line-front-space
   (:eval ,pspmacs/pspline-evil-state)
   (:eval ,pspmacs/pspline-buffer-state)
   (:eval ,pspmacs/pspline-major-icon)
   (:eval ,pspmacs/pspline-buffer-name)
   (:eval ,pspmacs/pspline-cursor-position)
   (:eval mode-line-misc-info)
   mode-line-format-right-align
   (:eval ,pspmacs/pspline-version-control)
   " "
   (:eval ,pspmacs/pspline-time)
   mode-line-end-spaces))

(provide 'pspmacs/pspline)
;;; pspline.el ends there

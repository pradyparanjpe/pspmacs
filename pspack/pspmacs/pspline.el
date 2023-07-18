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

(defface pspmacs/pspline-buffer-modified-face
  '((t (:foreground "#ff8f9f")))
  "Face of buffer name when buffer is modified"
  :group 'pspline)

(defface pspmacs/pspline-win-loc-face
  '((t (:foreground "#5fafff")))
  "Face of window location indicator"
  :group 'pspline)

(defface pspmacs/pspline-cursor-position-face
  '((t (:foreground "#cfcf3f")))
  "Face of cursor position row:col indicator"
  :group 'pspline)

(defface pspmacs/pspline-vc-main-face
  '((t (:foreground "#ff7f3f")))
  "Face of buffer name when buffer is state"
  :group 'pspline)

(defface pspmacs/pspline-vc-release-face
  '((t (:foreground "#7f3fff")))
  "Face of vc release branch"
  :group 'pspline)

(defface pspmacs/pspline-vc-non-main-face
  '((t (:foreground "#7fff3f")))
  "Face of vc non-main branch"
  :group 'pspline)

(defface pspmacs/pspline-evil-normal-face
  '((t (:foreground "#ff9f00")))
  "Normal evil state")

(defface pspmacs/pspline-evil-insert-face
  '((t (:foreground "#00ff9f")))
  "Evil insert state")

(defface pspmacs/pspline-evil-visual-face
  '((t (:foreground "#009fff")))
  "Evil visual state")

(defface pspmacs/pspline-evil-operator-face
  '((t (:foreground "#ff009f")))
  "Evil operator state")

(defface pspmacs/pspline-evil-motion-face
  '((t (:foreground "#3fffff")))
  "Evil Motion state")

(defface pspmacs/pspline-evil-emacs-face
  '((t (:foreground "#bfbfbf")))
  "Emacs evil state")

(defface pspmacs/pspline-evil-unknown-face
  '((t (:foreground "#000000")))
  "Unknown evil state")

(defvar pspmacs/pspline-major-icon
  '(:eval
     (when (display-graphic-p)
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
        " ")))

  "Major mode icon")

(defvar pspmacs/pspline-buffer-name
  '(:eval
     (let* ((base (if (buffer-modified-p)
                      'pspmacs/pspline-buffer-modified-face
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
        " ")))

  "Buffer-name, process-state")

(defvar pspmacs/pspline-win-loc
  '(:eval
     (propertize "%3p/%5I "
                 'face (if (pspmacs/pspline-buffer-focused-p)
                           'pspmacs/pspline-win-loc-face
                         'mode-line-inactive)))

  "Location of window in buffer")

(defvar pspmacs/pspline-cursor-position
  '(:eval
     (concat
      (propertize "%02l:%02c "
                  'face (if (pspmacs/pspline-buffer-focused-p)
                            'pspmacs/pspline-cursor-position-face
                          'mode-line-inactive))))

  "Cursor: row:col")

(defvar pspmacs/pspline-evil-state
  '(:eval
    (propertize " "
                'face
                (if (pspmacs/pspline-buffer-focused-p)
                    (case evil-state
                      (normal 'pspmacs/pspline-evil-normal-face)
                      (insert 'pspmacs/pspline-evil-insert-face)
                      (visual 'pspmacs/pspline-evil-visual-face)
                      (replace 'pspmacs/pspline-evil-replace-face)
                      (operator 'pspmacs/pspline-evil-operator-face)
                      (motion 'pspmacs/pspline-evil-motion-face)
                      (emacs 'pspmacs/pspline-evil-emacs-face)
                      (_ 'pspmacs/pspline-evil-emacs-face))
                  'mode-line-inactive)
                'help-echo
                (symbol-name evil-state)))

  "Evil state dot")

(defvar pspmacs/pspline-info
  '(:eval mode-line-misc-info)
  "handle for miscellaneous information")

(defvar pspmacs/pspline-version-control
  '(:eval (when (stringp vc-mode)
            (let
                ((vc-spec
                  (replace-regexp-in-string
                   (format "^ %s[:-]" (vc-backend buffer-file-name))
                   " " vc-mode)))
              (propertize (concat vc-spec " ")
                          'face
                          (pcase
                              vc-spec
                            (" main" 'pspmacs/pspline-vc-main-face)
                            (" master" 'pspmacs/pspline-vc-main-face)
                            (" release" 'pspmacs/pspline-vc-release-face)
                            (_ 'pspmacs/pspline-vc-non-main-face))))))
  "version control spec")

(defvar pspmacs/pspline-time
  '(:eval
     (propertize (format-time-string "⏲%H:%M")
                 'face 'bold 'help-echo (format-time-string "%c")))

  "Time segment")

(defcustom pspmacs/pspline-segments-plist
  '((pspmacs/pspline-evil-state . (t nil))
    (pspmacs/pspline-cursor-position . (t nil))
    (pspmacs/pspline-win-loc . (t nil))
    (pspmacs/pspline-major-icon . (t nil))
    (pspmacs/pspline-version-control . (t nil))
    (pspmacs/pspline-buffer-name . (t nil))
    (pspmacs/pspline-info . (t t))
    (pspmacs/pspline-time . (t t)))
  "Ordered alist whose car is segment handles and cdr is lists of the form
  '(show on-right)
When SHOW is non-nil, we display the segment on mode-line
When ON-RIGHT is non-nil, the segment is aligned from the right.
")

(defun pspmacs/pspline-buffer-focused-p ()
  "Is the cognate buffer focused?"
  (eq (current-buffer) (window-buffer (selected-window))))

(defun pspmacs/pspline-order ()
  "Construct pspline-order"
  (let* ((left-segs nil)
         (right-segs nil))
    (dolist (seg pspmacs/pspline-segments-plist nil)
      (print (car seg))
      (if (nth 0 (cdr seg))
          (if (nth 1 (cdr seg))
              (add-to-list 'right-segs (eval (car seg)) t)
            (add-to-list 'left-segs (eval (car seg)) t))))
    `("%e"
      mode-line-front-space
      ,@left-segs
      mode-line-format-right-align
      ,@right-segs
      mode-line-end-spaces)))

(provide 'pspmacs/pspline)
;;; pspline.el ends there

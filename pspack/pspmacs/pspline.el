;;; pspline.el --- pspline modeline -*- lexical-binding: t; -*-
;;; Commentary:
;;
;; PSPLINE ModeLine for Emacs
;;
;;; Code:
(defgroup pspline nil
  "PSPLINE: modeline for pspmacs."
  :group 'pspmacs)

(use-package all-the-icons
  :if (display-graphic-p))

(use-package battery
  :ensure nil
  :commands battery-upower)

(defcustom pspmacs/pspline-all-the-icons-installed-p
  nil
  "`all-the-icons-install-fonts' was called"
  :type 'boolean
  :group 'pspline)

(defcustom pspmacs/pspline-win-loc-format
  '(concat (eval pspmacs/pspline-loc-pc-format) "/%05I")

  "window location string (at:of)"
  :type '(choice
          (string :tag "verbatim")
          (sexp :tag "Evaluates to string"))
  :group 'pspline)

(defcustom pspmacs/pspline-cursor-position-format
  "%03l:%02c"

  "Cursor position string (row:col)"
  :type '(choice
          (string :tag "verbatim")
          (sexp :tag "Evaluates to string"))
  :group 'pspline)

(defcustom pspmacs/pspline-evil-state-format
  ""

  "Evil state (vim keybindings) indicator"
  :type '(choice
          (string :tag "verbatim")
          (sexp :tag "Evaluates to string"))
  :group 'pspline)

(defcustom pspmacs/pspline-time-string-format
  "%H:%M"

  "window location format"
  :type '(string :tag "Time string format")
  :group 'pspline)

(defcustom pspmacs/pspline--show-string
  "percent"
  "Type of information to show as battery"
  :type '(string :options ("time" "percent"))
  :group 'pspline)

(defcustom pspmacs/pspline-battery-icon-plist
  '((90 . "\uf240")
    (66 . "\uf241")
    (33 . "\uf242")
    (10 . "\uf243")
    (0  . "\uf244"))
  "Battery icon cdr for battery-percentage above car"
  :type '(repeat (cons (number :tag "Icon above")
                       (string :tag "Icon")))
  :group 'pspline)

(defcustom pspmacs/pspline-battery-color-plist
  '((101 . "#00cfcf")
    (75  . "#00cf00")
    (50  . "#cfcf00")
    (25  . "#cf4f00")
    (0   . "#cf0000"))
  "Color cdr for battery-percentage above car
car >101 is interpreted as *charging*"
  :type '(repeat (cons (number :tag "Icon above")
                       (string :tag "color-string")))
  :group 'pspline)

(defface pspmacs/pspline-buffer-modified-face
  '((t (:foreground "#cf5f6f")))

  "Face of buffer name when buffer is modified"
  :group 'pspline)

(defface pspmacs/pspline-win-loc-face
  '((t (:foreground "#2f7fcf")))

  "Face of window location indicator"
  :group 'pspline)

(defface pspmacs/pspline-cursor-position-face
  '((t (:foreground "#ffff7f")))

  "Face of cursor position row:col indicator"
  :group 'pspline)

(defface pspmacs/pspline-vc-main-face
  '((t (:foreground "#cf4f0f")))

  "Face of buffer name when buffer is state"
  :group 'pspline)

(defface pspmacs/pspline-vc-release-face
  '((t (:foreground "#7f3fff")))

  "Face of vc release branch"
  :group 'pspline)

(defface pspmacs/pspline-vc-non-main-face
  '((t (:foreground "#4fcf0f")))

  "Face of vc non-main branch"
  :group 'pspline)

(defface pspmacs/pspline-evil-normal-face
  '((t (:foreground "#ff9f00")))

  "Normal evil state"
  :group 'pspline)

(defface pspmacs/pspline-evil-insert-face
  '((t (:foreground "#00cf6f")))

  "Evil insert state"
  :group 'pspline)

(defface pspmacs/pspline-evil-visual-face
  '((t (:foreground "#009fff")))

  "Evil visual state"
  :group 'pspline)

(defface pspmacs/pspline-evil-operator-face
  '((t (:foreground "#ff009f")))

  "Evil operator state"
  :group 'pspline)

(defface pspmacs/pspline-evil-motion-face
  '((t (:foreground "#3fffff")))

  "Evil Motion state"
  :group 'pspline)

(defface pspmacs/pspline-evil-emacs-face
  '((t (:foreground "#bfbfbf")))

  "Emacs evil state"
  :group 'pspline)

(defface pspmacs/pspline-evil-unknown-face
  '((t (:foreground "#000000")))

  "Unknown evil state"
  :group 'pspline)

(defface pspmacs/pspline-time-face
  '((t (:bold t :foreground "#af8f00")))
  "Pspline time face"
  :group 'pspline)

(defun pspmacs/pspline--major-icon ()
  "evaluated by `pspmacs/pspline-major-icon'."
  (pspmacs/pspline--assert-all-the-icons)
  (when (display-graphic-p)
    (concat
     (propertize
      (let*
          ((icon (ignore-errors
                   (all-the-icons-icon-for-buffer)))
           (icon (if icon
                     icon
                   (ignore-errors
                     (all-the-icons-icon-for-mode major-mode)))))
        icon)
      'help-echo
      (capitalize (string-trim (symbol-name major-mode) nil "-mode")))
     " ")))

(defvar pspmacs/pspline-major-icon
  '(:eval (pspmacs/pspline--major-icon))
  "Major mode icon.")

(defun pspmacs/pspline--toggle-read-only (&optional _button)
  "Toggle read-only-mode"
  (read-only-mode 'toggle)
  (force-mode-line-update t))

(defun pspmacs/pspline--buffer-name ()
  "evaluated by `pspmacs/pspline--buffer-name'."
  (let* ((base (if (buffer-modified-p)
                   'pspmacs/pspline-buffer-modified-face
                 (if (pspmacs/pspline-buffer-focused-p)
                     'mode-line-buffer-id
                   'mode-line-inactive)))
         (box (if buffer-read-only '(:box t) '(:box nil)))
         (buffer-string (concat (or
                         (ignore-errors
                           (file-relative-name buffer-file-name
                                               (projectile-project-mode)))
                         "%b")
                                " ")))
     (propertize
      (buttonize buffer-string #'pspmacs/pspline--toggle-read-only)
      'face `(,base ,box)
      'help-echo "mouse-1 toggle read-only")))

(defvar pspmacs/pspline-buffer-name
  '(:eval (pspmacs/pspline--buffer-name))
  "Buffer-name, process-state.
Customize face with `pspmacs/pspline-buffer-modified-face'.")

(defun pspmacs/pspline--buffer-process ()
  "evaluated by `pspmacs/pspline-buffer-process'."
  (if mode-line-process
      (propertize (format "%s " mode-line-process)
                  'face '(:foreground modeline-info :box t))))

  (defvar pspmacs/pspline-buffer-process
    '(:eval (pspmacs/pspline--buffer-process))
    "Buffer-process.")

(defun pspmacs/pspline--win-loc ()
  "evaluated by `pspmacs/pspline-win-loc'."
  (propertize (concat (eval pspmacs/pspline-win-loc-format) " ")
              'face (if (pspmacs/pspline-buffer-focused-p)
                        'pspmacs/pspline-win-loc-face
                      'mode-line-inactive)))

(defvar pspmacs/pspline-win-loc
  '(:eval (pspmacs/pspline--win-loc))
  "Location of window in buffer
Customize value with `pspmacs/pspline-win-loc-format'.
Customize face with `pspmacs/pspline-win-loc-face'.")

(defun pspmacs/pspline--cursor-position ()
  "evaluated by `pspmacs/pspline-cursor-position'."
  (concat
   (propertize (concat (eval pspmacs/pspline-cursor-position-format) " ")
               'face (if (pspmacs/pspline-buffer-focused-p)
                         'pspmacs/pspline-cursor-position-face
                       'mode-line-inactive))))

(defvar pspmacs/pspline-cursor-position
  '(:eval (pspmacs/pspline--cursor-position))
  "Cursor position indicator <row:col>.
Customize value with `pspmacs/pspline-cursor-position-format'.
Customize face with `pspmacs/pspline-cursor-position-face'.")

(defun pspmacs/pspline--evil-state ()
  "evaluated by `pspmacs/pspline-evil-state'"
  (propertize (concat (eval pspmacs/pspline-evil-state-format) " ")
              'face
              (if (pspmacs/pspline-buffer-focused-p)
                  (cl-case evil-state
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

(defvar pspmacs/pspline-evil-state
  '(:eval (pspmacs/pspline--evil-state))

  "Evil state dot
Customize faces with `pspmacs/pspline-evil-state-format',
`pspmacs/pspline-evil-normal-face',
`pspmacs/pspline-evil-insert-face',
`pspmacs/pspline-evil-visual-face',
`pspmacs/pspline-evil-replace-face',
`pspmacs/pspline-evil-operator-face',
`pspmacs/pspline-evil-motion-face',
`pspmacs/pspline-evil-emacs-face',
`pspmacs/pspline-evil-unknown-face'.")

(defvar pspmacs/pspline-info
  '(:eval mode-line-misc-info)

  "Handle for miscellaneous information")

(defun pspmacs/pspline--version-control()
  "evaluated by `pspmacs/pspline-version-control'."
  (when (stringp vc-mode)
    (let
        ((vc-spec
          (replace-regexp-in-string
           (format "^ %s[-:@]" (vc-backend buffer-file-name))
           " " vc-mode)))
      (propertize
       (concat vc-spec " ")
       'face
       (pcase
           vc-spec
         (" main" 'pspmacs/pspline-vc-main-face)
         (" master" 'pspmacs/pspline-vc-main-face)
         (" release" 'pspmacs/pspline-vc-release-face)
         (_ 'pspmacs/pspline-vc-non-main-face))))))

(defvar pspmacs/pspline-version-control
  '(:eval (pspmacs/pspline--version-control))

  "Version control spec.
Customize faces with `pspmacs/pspline-vc-main-face',
`pspmacs/pspline-vc-non-main-face',
`pspmacs/pspline-vc-release-face'.")

(defun pspmacs/pspline--time ()
  "evaluated by `pspmacs/pspline-time'."
  (propertize
   (concat
    (format-time-string (eval pspmacs/pspline-time-string-format))
    " ")
   'face 'pspmacs/pspline-time-face
   'help-echo (format-time-string "%c")))

(defvar pspmacs/pspline-time
  '(:eval (pspmacs/pspline--time))

  "Time segment.
Customize value with `pspmacs/pspline-time-string-format'.")

(defun pspmacs/pspline--battery-toggle-show-string (&optional _button)
  "Toggle display and help-text"
  (customize-set-variable
   'pspmacs/pspline--show-string
   (if (string= pspmacs/pspline--show-string "time")
       "percent"
     "time"))
  (force-mode-line-update t))

(defun pspmacs/pspline--battery-icon (perc)
  "Battery icon based on current battery percentage PERC"
  (cl-some (lambda (x)
             (if (> perc (car x)) (cdr x)))
           pspmacs/pspline-battery-icon-plist))

(defun pspmacs/pspline--battery-color (perc)
  "Battery color based on current battery percentage PERC

PERC > 101 is interpreted as *charging*"
  (cl-some (lambda (x)
             (if (> perc (car x)) (cdr x)))
           pspmacs/pspline-battery-color-plist))

(defun pspmacs/pspline--battery ()
  "evaluated by `pspmacs/pspline-battery'."
  (let* ((battery-info (funcall battery-status-function))
         (hours-remain (concat (cdr (assq ?t battery-info)) "h"))
         (bat-perc (cdr (assq ?p battery-info)))
         (bat-perc-num (if (stringp bat-perc)
                           (string-to-number bat-perc)
                         bat-perc))
         (bat-perc-string (format "%s%%" bat-perc-num))
         (bat-icon (pspmacs/pspline--battery-icon bat-perc-num))
         (bat-color (pspmacs/pspline--battery-color
                     (if (string= (cdr (assq ?b battery-info)) "+")
                         200
                       bat-perc-num)))
         (bat-string (concat bat-icon
                              (if (string= pspmacs/pspline--show-string "time")
                                  hours-remain
                                (format "%s%%" bat-perc-string))
                              " "))
         (tooltip-string (if (string= pspmacs/pspline--show-string "time")
                             bat-perc-string
                           hours-remain)))
    (propertize (buttonize bat-string
                           #'pspmacs/pspline--battery-toggle-show-string)
                'face
                `(:foreground ,bat-color)
                'help-echo
                tooltip-string
                'mouse-face
                `(:foreground "#000000" :background ,bat-color))))

(defvar pspmacs/pspline-battery
  '(:eval (pspmacs/pspline--battery))
  "Battery segment.
Customize value with `pspmacs/pspline-battery-icon-plist',
`pspmacs/pspline-battery-color-plist'.")

(defcustom pspmacs/pspline-segments-plist
  '((pspmacs/pspline-evil-state t nil)
    (pspmacs/pspline-cursor-position t nil)
    (pspmacs/pspline-win-loc t nil)
    (pspmacs/pspline-major-icon t nil)
    (pspmacs/pspline-version-control t nil)
    (pspmacs/pspline-buffer-name t nil)
    (pspmacs/pspline-buffer-process t nil)
    (pspmacs/pspline-info t t)
    (pspmacs/pspline-battery t t)
    (pspmacs/pspline-time t t))

  "Ordered list whose car is segment handle and cdr is '(show on-right)

When SHOW is non-nil, we display the segment on mode-line
When ON-RIGHT is non-nil, the segment is aligned from the right."
  :type '(repeat (list (symbol :tag "Evaluates to segment string")
                       (boolean :tag "Show this segment")
                       (boolean :tag "Align right"))))

(defun pspmacs/pspline--assert-all-the-icons ()
  (unless pspmacs/pspline-all-the-icons-installed-p
    (if (ignore-errors
          (all-the-icons-install-fonts t))
        (customize-save-variable
         'pspmacs/pspline-all-the-icons-installed-p)
      t)))

(defvar pspmacs/pspline-loc-pc-format
  '(or
    (ignore-errors
      (format "%3d%%%%"
              (let ((fend (/ (window-end) 0.01 (point-max)))
                    (fstart (/ (- (window-start) 1) 0.01 (point-max))))
                (if (= fstart 0)
                    (if (= fend 100)
                        nil
                      0)
                  fend))))
    " all")
  "Buffer location in percentage or all")

(defun pspmacs/pspline-buffer-focused-p ()
  "Is the cognate buffer focused?"

  (eq (current-buffer) (window-buffer (selected-window))))

(when (version< emacs-version "30")
  (defcustom mode-line-right-align-edge 'window
    "For forward compatibility with master branch version 30
Where function `mode-line-format-right-align' should align to.
Internally, that function uses `:align-to' in a display property,
so aligns to the left edge of the given area.  See info node
`(elisp)Pixel Specification'.

Must be set to a symbol.  Acceptable values are:
- `window': align to extreme right of window, regardless of margins
  or fringes
- `right-fringe': align to right-fringe
- `right-margin': align to right-margin"
    :type '(choice (const right-margin)
                   (const right-fringe)
                   (const window))
    :group 'mode-line
    :version "30.1")

  (defun mode--line-format-right-align ()
    "For forward compatibility with master branch version 30
Right-align all following mode-line constructs.

When the symbol `mode-line-format-right-align' appears in
`mode-line-format', return a string of one space, with a display
property to make it appear long enough to align anything after
that symbol to the right of the rendered mode line.  Exactly how
far to the right is controlled by `mode-line-right-align-edge'.

It is important that the symbol `mode-line-format-right-align' be
included in `mode-line-format' (and not another similar construct
such as `(:eval (mode-line-format-right-align)').  This is because
the symbol `mode-line-format-right-align' is processed by
`format-mode-line' as a variable."
    (let* ((rest (cdr (memq 'mode-line-format-right-align
                            mode-line-format)))
           (rest-str (format-mode-line `("" ,@rest)))
           (rest-width (progn
                         (add-face-text-property
                          0 (length rest-str) 'mode-line t rest-str)
                         (string-pixel-width rest-str))))
      (propertize " " 'display
                  ;; The `right' spec doesn't work on TTY frames
                  ;; when windows are split horizontally (bug#59620)
                  (if (and (display-graphic-p)
                           (not (eq mode-line-right-align-edge 'window)))
                      `(space :align-to (- ,mode-line-right-align-edge
                                           (,rest-width)))
                    `(space :align-to (,(- (window-pixel-width)
                                           (window-scroll-bar-width)
                                           (window-right-divider-width)
                                           (* (or (cdr (window-margins)) 1)
                                              (frame-char-width))
                                           ;; Manually account for value of
                                           ;; `mode-line-right-align-edge' even
                                           ;; when display is non-graphical
                                           (pcase mode-line-right-align-edge
                                             ('right-margin
                                              (or (cdr (window-margins)) 0))
                                             ('right-fringe
                                              ;; what here?
                                              (or (cadr (window-fringes)) 0))
                                             (_ 0))
                                           rest-width)))))))

  (defvar mode-line-format-right-align '(:eval (mode--line-format-right-align))
    "For forward compatibility with master branch version 30
Mode line construct to right align all following constructs.")
    ;;;###autoload
  (put 'mode-line-format-right-align 'risky-local-variable t))

(defun pspmacs/pspline-order ()
  "Construct pspline-order"
  (let* ((left-segs nil)
         (right-segs nil))
    (dolist (seg pspmacs/pspline-segments-plist nil)
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

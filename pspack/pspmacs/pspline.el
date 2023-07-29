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

(defvar pspmacs/pspline--original-format
  mode-line-format
  "Save original mode-line format")

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

(defcustom pspmacs/pspline-buffer-name-length
  20
  "Length of buffer beyond which, name is trimmed"
  :type 'number
  :group 'pspline)

(defcustom pspmacs/pspline-buffer-name-ellipses
  "…"
  "Character(s) that indicates that the name was trimmed"
  :type '(string)
  :group 'pspline)

(defcustom pspmacs/pspline-evil-state-format
  ""

  "Evil state (vim keybindings) indicator"
  :type '(choice
          (string :tag "verbatim")
          (sexp :tag "Evaluates to string"))
  :group 'pspline)

(defcustom pspmacs/pspline-flymake-error-icon
  (propertize " ⛌ " 'face 'pspmacs/pspline-flymake-error-face)
  "Flymake error icon"
  :type '(string :tag "Propertized with `pspmacs/pspline-flymake-error-face'")
  :group 'pspline)

(defcustom pspmacs/pspline-flymake-warning-icon
  (propertize " ! " 'face 'pspmacs/pspline-flymake-warning-face)
  "Flymake warning icon"
  :type '(string :tag "Propertized with `pspmacs/pspline-flymake-warning-face'")
  :group 'pspline)

(defcustom pspmacs/pspline-flymake-note-icon
  (propertize " ? " 'face 'pspmacs/pspline-flymake-note-face)
  "Flymake note icon"
  :type '(string :tag "Propertized with `pspmacs/pspline-flymake-note-face'")
  :group 'pspline)

(defcustom pspmacs/pspline-flymake-good-icon
  (propertize " 🗸 " 'face 'pspmacs/pspline-flymake-good-face)
  "Flymake good icon"
  :type '(string :tag "Propertized with `pspmacs/pspline-flymake-good-face'")
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

(defcustom pspmacs/pspline-segments-plist
  '((pspmacs/pspline-evil-state . (:display t :right nil :inactive nil))
    (pspmacs/pspline-cursor-position . (:display t :right nil :inactive t))
    (pspmacs/pspline-win-loc . (:display t :right nil :inactive t))
    (pspmacs/pspline-major-icon . (:display t :right nil :inactive t))
    (pspmacs/pspline-version-control . (:display t :right nil :inactive nil))
    (pspmacs/pspline-buffer-name . (:display t :right nil :inactive t))
    (pspmacs/pspline-buffer-process . (:display t :right nil :inactive t))
    (pspmacs/pspline-info . (:display t :right t :inactive nil))
    (pspmacs/pspline-error-hints . (:display t :right t :inactive nil))
    (pspmacs/pspline-battery . (:display t :right t :inactive nil))
    (pspmacs/pspline-time . (:display t :right t :inactive nil)))

  "Ordered list whose
car is segment handle
cdr is '(:show nil :on-right nil :inactive nil)

When :SHOW is non-nil, display the segment on mode-line
When :RIGHT is non-nil, align the setment from the right.
When :INACTIVE is non-nil, display the segment even in inactive buffer"
  :type '(repeat (cons (symbol :tag "Evaluates to segment string")
                       (plist :key-type
                              (symbol :options '(:display :right :inactive))
                              :value-type boolean)))
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

(defface pspmacs/pspline-vc-non-main-face
  '((t (:foreground "#4fcf0f")))

  "Face of vc non-main branch"
  :group 'pspline)

(defface pspmacs/pspline-vc-release-face
  '((t (:foreground "#7f3fff")))

  "Face of vc release branch"
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

(defface pspmacs/pspline-evil-replace-face
  '((t (:foreground "#ffff00")))

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

(defface pspmacs/pspline-flymake-error-face
  '((t (:foreground "#cf0f8f")))
  "Face of Flymake Error Counter"
  :group 'pspline)

(defface pspmacs/pspline-flymake-warning-face
  '((t (:foreground "#cf8f0f")))
  "Face of Flymake Error Counter"
  :group 'pspline)

(defface pspmacs/pspline-flymake-note-face
  '((t (:foreground "#0fcf8f")))
  "Face of Flymake Error Counter"
  :group 'pspline)

(defface pspmacs/pspline-flymake-good-face
  '((t (:foreground "#0f8fcf")))
  "Face of Flymake Error Counter"
  :group 'pspline)

(defface pspmacs/pspline-time-face
  '((t (:foreground "#df00ff")))

  "Pspline time face"
  :group 'pspline)

(defun pspmacs/pspline--major-icon ()
  "evaluated by `pspmacs/pspline-major-icon'."
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
  '(:eval (if (pspmacs/pspline--display-segment
               'pspmacs/pspline-major-icon)
              (pspmacs/pspline--major-icon)))
  "Major mode icon.")

(defun pspmacs/pspline--toggle-read-only (&optional _button)
  "Toggle read-only-mode"
  (read-only-mode 'toggle)
  (force-mode-line-update t))

(defun pspmacs/pspline--shorten (buffer-name)
  "Shorten buffer name"
  (let* ((buffer-mid (/ (length buffer-name) 2))
         (buffer-cut
          (1+ (- buffer-mid (/ pspmacs/pspline-buffer-name-length 2)))))
    (if (cl-plusp buffer-cut)
        (concat (substring buffer-name 0 (- buffer-mid buffer-cut))
                pspmacs/pspline-buffer-name-ellipses
                (substring buffer-name (+ buffer-mid buffer-cut)))
      buffer-name)))

(defun pspmacs/pspline--buffer-name ()
  "evaluated by `pspmacs/pspline--buffer-name'."
  (let* ((base (if (buffer-modified-p)
                   'pspmacs/pspline-buffer-modified-face
                 (if (pspmacs/pspline--buffer-focused-p)
                     'mode-line-buffer-id
                   'mode-line-inactive)))
         (box (if buffer-read-only '(:box t) '(:box nil)))
         (buffer-string
          (concat (or
                   (ignore-errors
                     (file-relative-name buffer-file-name
                                         (projectile-project-mode)))
                   "%b")
                  " ")))
    (propertize
     (buttonize (pspmacs/pspline--shorten buffer-string)
                #'pspmacs/pspline--toggle-read-only)
     'face `(,base ,box)
     'help-echo "mouse-1 toggle read-only")))

(defvar pspmacs/pspline-buffer-name
  '(:eval (if (pspmacs/pspline--display-segment
               'pspmacs/pspline-buffer-name)
              (pspmacs/pspline--buffer-name)))
  "Buffer-name, process-state.
Customize face with `pspmacs/pspline-buffer-modified-face'.")

(defun pspmacs/pspline--buffer-process ()
  "evaluated by `pspmacs/pspline-buffer-process'."
  (if mode-line-process
    (propertize (pspmacs/pspline--shorten (format "%s " mode-line-process))
                'face '(:foreground
                        (modus-themes-get-color-value 'modeline-info)
                        :box t))))

(defvar pspmacs/pspline-buffer-process
  '(:eval (if (pspmacs/pspline--display-segment
               'pspmacs/pspline-buffer-process)
              (pspmacs/pspline--buffer-process)))
  "Buffer-process.")

(defun pspmacs/pspline--win-loc ()
  "evaluated by `pspmacs/pspline-win-loc'."
  (propertize (concat (eval pspmacs/pspline-win-loc-format) " ")
              'face (if (pspmacs/pspline--buffer-focused-p)
                        'pspmacs/pspline-win-loc-face
                      'mode-line-inactive)))

(defvar pspmacs/pspline-win-loc
  '(:eval (if (pspmacs/pspline--display-segment
               'pspmacs/pspline-win-loc)
              (pspmacs/pspline--win-loc)))
  "Location of window in buffer
Customize value with `pspmacs/pspline-win-loc-format'.
Customize face with `pspmacs/pspline-win-loc-face'.")

(defun pspmacs/pspline--cursor-position ()
  "evaluated by `pspmacs/pspline-cursor-position'."
  (concat
   (propertize (concat (eval pspmacs/pspline-cursor-position-format) " ")
               'face (if (pspmacs/pspline--buffer-focused-p)
                         'pspmacs/pspline-cursor-position-face
                       'mode-line-inactive))))

(defvar pspmacs/pspline-cursor-position
  '(:eval (if (pspmacs/pspline--display-segment
               'pspmacs/pspline-cursor-position)
              (pspmacs/pspline--cursor-position)))
  "Cursor position indicator <row:col>.
Customize value with `pspmacs/pspline-cursor-position-format'.
Customize face with `pspmacs/pspline-cursor-position-face'.")

(defun pspmacs/pspline--evil-state ()
  "evaluated by `pspmacs/pspline-evil-state'"
  (propertize (concat (eval pspmacs/pspline-evil-state-format) " ")
              'face
              (if (pspmacs/pspline--buffer-focused-p)
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
  '(:eval (if (pspmacs/pspline--display-segment
               'pspmacs/pspline-evil-statr)
              (pspmacs/pspline--evil-state)))

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

(defun pspmacs/pspline--info (&optional show-always
    mode-line-misc-info))

(defvar pspmacs/pspline-info
  '(:eval (if (pspmacs/pspline--display-segment
               'pspmacs/pspline-info)
              (pspmacs/pspline--info)))

  "Handle for miscellaneous information")

(defun pspmacs/pspline--version-control ()
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
       (if (pspmacs/pspline--buffer-focused-p)
           (pcase
               vc-spec
             (" main" 'pspmacs/pspline-vc-main-face)
             (" master" 'pspmacs/pspline-vc-main-face)
             (" release" 'pspmacs/pspline-vc-release-face)
             (_ 'pspmacs/pspline-vc-non-main-face))
         'mode-line-inactive)))))

(defvar pspmacs/pspline-version-control
  '(:eval (if (pspmacs/pspline--display-segment
               'pspmacs/pspline-version-control)
              (pspmacs/pspline--version-control)))

  "Version control spec.
Customize faces with `pspmacs/pspline-vc-main-face',
`pspmacs/pspline-vc-non-main-face',
`pspmacs/pspline-vc-release-face'.")

(defun pspmacs/pspline--flymake-counter (type)
  (let ((count 0))
    (dolist (d (flymake-diagnostics))
      (when (= (flymake--severity type)
               (flymake--severity (flymake-diagnostic-type d)))
        (cl-incf count)))
    count))

(defun pspmacs/pspline--error-hints ()
  "Evaluated by `pspmacs/pspline-error-hints'."
  (let ((errr (pspmacs/pspline--flymake-counter :error))
        (wrng (pspmacs/pspline--flymake-counter :warning))
        (note (pspmacs/pspline--flymake-counter :note)))
    (concat
     (cond ((cl-plusp errr) pspmacs/pspline-flymake-error-icon)
           ((cl-plusp wrng) pspmacs/pspline-flymake-warning-icon)
           ((cl-plusp note) pspmacs/pspline-flymake-note-icon)
           (t pspmacs/pspline-flymake-good-icon))
     (if (cl-plusp errr)
         (propertize (format "%d " errr)
                     'face 'pspmacs/pspline-flymake-error-face))
     (if (cl-plusp wrng)
         (propertize (format "%d " wrng)
                     'face 'pspmacs/pspline-flymake-warning-face))
     (if (cl-plusp note)
         (propertize (format "%d " note)
                     'face 'pspmacs/pspline-flymake-note-face)))))

(defvar pspmacs/pspline-error-hints
  '(:eval (if (pspmacs/pspline--display-segment
               'pspmacs/pspline-error-hints)
              (pspmacs/pspline--error-hints)))

  "Version control spec.
Customize faces with")

(defun pspmacs/pspline--time ()
  "evaluated by `pspmacs/pspline-time'."
  (propertize
   (concat
    (format-time-string (eval pspmacs/pspline-time-string-format))
    " ")
   'face 'pspmacs/pspline-time-face
   'help-echo (format-time-string "%c")))

(defvar pspmacs/pspline-time
  '(:eval (if (pspmacs/pspline--display-segment
               'pspmacs/pspline-time)
              (pspmacs/pspline--time)))

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
  (when perc
    (let* ((red (* 0.008125 (* 2 (- 50 (max 0 (- perc 50))))))
           (green (* 0.008125 (* 2 (- 50 (max 0 (- 50 perc))))))
           (blue (* 0.008125 (* 10 (max 0 (- perc 90))))))
      (color-rgb-to-hex red green blue 2))))

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
         (bat-color (pspmacs/pspline--battery-color bat-perc-num))
         (bat-string (concat bat-icon
                             (if (string= pspmacs/pspline--show-string "time")
                                 hours-remain
                               (format "%s%%" bat-perc-string))
                             " "))
         (tooltip-string (if (string= pspmacs/pspline--show-string "time")
                             bat-perc-string
                           hours-remain)))
    (when bat-color
      (propertize (buttonize bat-string
                             #'pspmacs/pspline--battery-toggle-show-string)
                  'face
                  (if (string= (cdr (assq ?b battery-info)) "+")
                      `(:background ,bat-color)
                    `(:foreground ,bat-color))
                  'help-echo
                  tooltip-string
                  'mouse-face
                  `(:foreground "#000000" :background ,bat-color)))))

(defvar pspmacs/pspline-battery
  '(:eval (if (pspmacs/pspline--display-segment
               'pspmacs/pspline-battery)
              (pspmacs/pspline--battery)))
  "Battery segment.
Customize value with `pspmacs/pspline-battery-icon-plist',
`pspmacs/pspline-battery-color-plist'.")

(defun pspmacs/pspline--display-segment (seg-symbol)
  "Whether SEG-SYMBOL should be displayed"
  (or (pspmacs/pspline--buffer-focused-p)
      (cl-some (lambda (x)
                 (if (eq (car x) seg-symbol)
                     (plist-get (cdr x) :inactive)))
               pspmacs/pspline-segments-plist)))

(defun pspmacs/pspline--assert-all-the-icons ()
(with-eval-after-load
    custom-file
  (unless pspmacs/pspline-all-the-icons-installed-p
    (if (ignore-errors
          (all-the-icons-install-fonts t))
        (customize-save-variable
         'pspmacs/pspline-all-the-icons-installed-p
         t)))))

(defvar pspmacs/pspline-loc-pc-format
  '(or (ignore-errors
         (format "%3d%%%%"
                 (let ((fend (/ (window-end) 0.01 (point-max)))
                       (fstart (/ (- (window-start) 1) 0.01 (point-max))))
                   (if (= fstart 0) (if (= fend 100) nil 0) fend))))
       " all")
  "Buffer location in percentage or all")

(defvar pspmacs/pspline--current-window
  (frame-selected-window)
  "Current window.")

(defun pspmacs/pspline--set-selected-window (&rest _)
  "Set `pspmacs/pspline--current-window' appropriately."
  (let ((win (frame-selected-window)))
    (setq pspmacs/pspline--current-window
          (if (minibuffer-window-active-p win)
              (minibuffer-selected-window)
            win))))

(add-hook 'pre-redisplay-functions #'pspmacs/pspline--set-selected-window)

(defun pspmacs/pspline--buffer-focused-p ()
  "Is the cognate buffer focused?"
  (let ((fsw (frame-selected-window)))
    (and fsw (eq fsw pspmacs/pspline--current-window))))

(defun pspmacs/pspline-generate ()
  "Generate format of pspline.

If current buffer is not focused,
only display segments meant for inactive buffer"
  (let* ((left-segs nil)
         (right-segs nil))
    (dolist (seg pspmacs/pspline-segments-plist nil)
      (if (plist-get (cdr seg) :display)
          (let ((segment (eval (car seg))))
            (if (plist-get (cdr seg) :right)
                (add-to-list 'right-segs segment t)
              (add-to-list 'left-segs segment t)))))
    ;; Mode line format
    `("%e"
      mode-line-front-space
      ,@left-segs
      mode-line-format-right-align
      ,@right-segs
      mode-line-end-spaces)))

(defun pspmacs/pspline-reset ()
  "Reset pspline as default mode-line

When setting for first time, use `pspmacs/pspline-set-up'.
"
  (interactive)
  (let ((pspline-format (pspmacs/pspline-generate)))
    (setq-default mode-line-format pspline-format)
    (dolist (open-buff (buffer-list) nil)
      (with-current-buffer open-buff
        (setq mode-line-format (pspmacs/pspline-generate)))))
  (pspmacs/pspline--assert-all-the-icons))

(defun pspmacs/pspline-set-up ()
  "Set up pspline as mode-line

Save current `mode-line-format' as `pspmacs/pspline--original-format'
To reset, consider `pspmacs/pspline-reset'
"
  (interactive)
  (setq pspmacs/pspline--original-format mode-line-format)
  (pspmacs/pspline-reset))

(defun pspmacs/pspline-tear-down ()
  "tear down pspline as mode-line, resetting to
`pspmacs/pspline--original-format'"
  (interactive)
  (setq-default mode-line-format  pspmacs/pspline--original-format)
  (dolist (open-buff (buffer-list) nil)
    (with-current-buffer open-buff
      (setq mode-line-format pspmacs/pspline--original-format))))

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

(provide 'pspmacs/pspline)
;;; pspline.el ends there

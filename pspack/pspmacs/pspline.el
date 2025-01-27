;;; pspline.el --- pspline modeline -*- lexical-binding: t; -*-
;;; Commentary:
;;
;; PSPLINE ModeLine for Emacs
;;
;;; Code:
(defgroup pspline nil
  "PSPLINE: modeline for pspmacs."
  :group 'pspmacs)

(require 'pspmacs/common)
(use-package all-the-icons
  :if (display-graphic-p))

(use-package battery
  :ensure nil
  :commands battery-upower)

(defvar pspmacs/pspline--original-format
  mode-line-format
  "Save original mode-line format.")

(defcustom pspmacs/pspline-all-the-icons-installed-p nil
  "Was `all-the-icons-install-fonts' called."
  :type 'boolean
  :group 'pspline)

(defcustom pspmacs/pspline-win-loc-format
  '(concat (eval pspmacs/pspline-loc-pc-format) "/%05I")
  "Window location string (at:of)."
  :type '(choice
          (string :tag "verbatim")
          (sexp :tag "Evaluates to string"))
  :group 'pspline)

(defcustom pspmacs/pspline-cursor-pos-format "%03l:%02c"
  "Cursor position string (row:col)."
  :type '(choice
          (string :tag "verbatim")
          (sexp :tag "Evaluates to string"))
  :group 'pspline)

(defcustom pspmacs/pspline-evil-state-format ""
  "Evil state (vim keybindings) indicator."
  :type '(choice
          (string :tag "verbatim")
          (sexp :tag "Evaluates to string"))
  :group 'pspline)

(defcustom pspmacs/pspline-input-method-replace-alist nil
  "Replace window-location `mode-line' indicator string with custom string."
  :type '(repeat (cons (string :tag "mode-line indicator string")
                       (string :tag "replacement")))
  :group 'pspline)

(defcustom pspmacs/pspline-buffer-name-length 20
  "Length of buffer name beyond which, it is trimmed."
  :type 'number
  :group 'pspline)

(defcustom pspmacs/pspline-flymake-error-icon
  (propertize " × " 'face 'pspmacs/pspline-flymake-error)
  "Flymake error icon."
  :type '(string :tag "Propertized with `pspmacs/pspline-flymake-error'")
  :group 'pspline)

(defcustom pspmacs/pspline-flymake-warning-icon
  (propertize " ! " 'face 'pspmacs/pspline-flymake-warning)
  "Flymake warning icon."
  :type '(string :tag "Propertized with `pspmacs/pspline-flymake-warning'")
  :group 'pspline)

(defcustom pspmacs/pspline-flymake-note-icon
  (propertize " ? " 'face 'pspmacs/pspline-flymake-note)
  "Flymake note icon."
  :type '(string :tag "Propertized with `pspmacs/pspline-flymake-note'")
  :group 'pspline)

(defcustom pspmacs/pspline-flymake-good-icon
  (propertize " 🗸 " 'face 'pspmacs/pspline-flymake-good)
  "Flymake good icon."
  :type '(string :tag "Propertized with `pspmacs/pspline-flymake-good'")
  :group 'pspline)

(defcustom pspmacs/pspline-time-string-format "%H:%M"
  "Window location format."
  :type '(string :tag "Time string format")
  :group 'pspline)

(defcustom pspmacs/pspline--show-string "percent"
  "Type of information to show as battery."
  :type '(string :options ("time" "percent"))
  :group 'pspline)

(defcustom pspmacs/pspline-battery-icon-plist
  '((90 . "\uf240")
    (66 . "\uf241")
    (33 . "\uf242")
    (10 . "\uf243")
    (0  . "\uf244"))
  "Battery icon cdr for battery-percentage above car."
  :type '(repeat (cons (number :tag "Icon above")
                       (string :tag "Icon")))
  :group 'pspline)

(defcustom pspmacs/pspline-segments-alist
  '((pspmacs/pspline-evil-state . (:display t :right nil :inactive nil))
    (pspmacs/pspline-cursor-pos . (:display t :right nil :inactive t))
    (pspmacs/pspline-win-loc . (:display t :right nil :inactive t))
    (pspmacs/pspline-input-method . (:display t :right nil :inactive nil))
    (pspmacs/pspline-major-icon . (:display t :right nil :inactive t))
    (pspmacs/pspline-version-control . (:display t :right nil :inactive nil))
    (pspmacs/pspline-buffer-name . (:display t :right nil :inactive t))
    (pspmacs/pspline-buffer-process . (:display t :right nil :inactive t))
    (pspmacs/pspline-info . (:display t :right t :inactive nil))
    (pspmacs/pspline-error-hints . (:display t :right t :inactive nil))
    (pspmacs/pspline-battery . (:display t :right t :inactive nil))
    (pspmacs/pspline-time . (:display t :right t :inactive nil)))

  "Ordered alist of segment with properties.

car is segment handle
cdr is \\='(:display nil :right nil :inactive nil)

When :DISPLAY is non-nil, display the segment on mode-line
When :RIGHT is non-nil, align the setment from the right.
When :INACTIVE is non-nil, display the segment even in inactive buffer"
  :type '(repeat (cons (symbol :tag "Evaluates to segment string")
                       (plist :key-type
                              (symbol :options '(:display :right :inactive))
                              :value-type boolean)))
  :group 'pspline)

(defface pspmacs/pspline-buffer-modified '((default (:foreground "#cf5f6f")))
  "Face of buffer name when buffer is modified."
  :group 'pspline)

(defface pspmacs/pspline-buffer-process
  '((default (:foreground "#9fefff" :box t)))
  "Face of buffer process."
  :group 'pspline)

(defface pspmacs/pspline-win-loc '((default (:foreground "#2f7fcf")))
  "Face of window location indicator."
  :group 'pspline)

(defface pspmacs/pspline-cursor-pos '((default (:foreground "#ffff7f")))
  "Face of cursor position row:col indicator."
  :group 'pspline)

(defface pspmacs/pspline-vc-main '((default (:foreground "#cf4f0f")))
  "Face of buffer name when buffer is state."
  :group 'pspline)

(defface pspmacs/pspline-vc-non-main '((default (:foreground "#4fcf0f")))
  "Face of vc non-main branch."
  :group 'pspline)

(defface pspmacs/pspline-vc-release '((default (:foreground "#7f3fff")))
  "Face of vc release branch."
  :group 'pspline)

(defface pspmacs/pspline-evil-normal '((default (:foreground "#ff9f00")))
  "Normal evil state."
  :group 'pspline)

(defface pspmacs/pspline-evil-insert '((default (:foreground "#00cf6f")))
  "Evil insert state."
  :group 'pspline)

(defface pspmacs/pspline-evil-visual '((default (:foreground "#009fff")))
  "Evil visual state."
  :group 'pspline)

(defface pspmacs/pspline-evil-replace '((default (:foreground "#ffff00")))
  "Evil visual state."
  :group 'pspline)

(defface pspmacs/pspline-evil-operator '((default (:foreground "#ff009f")))
  "Evil operator state."
  :group 'pspline)

(defface pspmacs/pspline-evil-motion '((default (:foreground "#3fffff")))
  "Evil Motion state."
  :group 'pspline)

(defface pspmacs/pspline-evil-emacs '((default (:foreground "#bfbfbf")))
  "Emacs evil state."
  :group 'pspline)

(defface pspmacs/pspline-evil-unknown '((default (:foreground "#000000")))
  "Unknown evil state."
  :group 'pspline)

(defface pspmacs/pspline-input-method '((default (:foreground "#af5fff")))
  "Face of input-method indicator."
  :group 'pspline)

(defface pspmacs/pspline-flymake-error '((default (:foreground "#cf0f8f")))
  "Face of Flymake Error Counter."
  :group 'pspline)

(defface pspmacs/pspline-flymake-warning '((default (:foreground "#cf8f0f")))
  "Face of Flymake Error Counter."
  :group 'pspline)

(defface pspmacs/pspline-flymake-note '((default (:foreground "#0fcf8f")))
  "Face of Flymake Error Counter."
  :group 'pspline)

(defface pspmacs/pspline-flymake-good '((default (:foreground "#0f8fcf")))
  "Face of Flymake Error Counter."
  :group 'pspline)

(defface pspmacs/pspline-time '((default (:foreground "#df00ff")))
  "Pspline time face."
  :group 'pspline)

(defun pspmacs/pspline--major-icon ()
  "Evaluated by `pspmacs/pspline-major-icon'."
  (when-let
      (((pspmacs/pspline--display-segment 'pspmacs/pspline-major-icon))
       (icon
        (if (string= major-mode 'eat-mode)
            (ignore-errors (all-the-icons-icon-for-mode 'vterm-mode))
          (or (ignore-errors (all-the-icons-icon-for-buffer))
              (ignore-errors (all-the-icons-icon-for-mode major-mode))))))
    `(,(propertize
        icon
        'help-echo
        (capitalize (string-trim (symbol-name major-mode) nil "-mode")))
      " ")))

(defvar-local pspmacs/pspline-major-icon '(:eval (pspmacs/pspline--major-icon))
  "Major mode icon.")

(defun pspmacs/pspline--toggle-read-only (&optional _button)
  "Toggle `read-only-mode'."
  (read-only-mode 'toggle)
  (force-mode-line-update t))

(defun pspmacs/pspline--buffer-name ()
  "Evaluated by `pspmacs/pspline--buffer-name'."
  (when-let (((pspmacs/pspline--display-segment 'pspmacs/pspline-buffer-name))
             (base (if (buffer-modified-p) 'pspmacs/pspline-buffer-modified
                     (if (mode-line-window-selected-p) 'mode-line-buffer-id
                       'mode-line-inactive)))
             (box (if buffer-read-only '(:box t) '(:box nil)))
             (buffer-string
              (or (ignore-errors
                    (file-relative-name
                     buffer-file-name (projectile-project-mode)))
                  "%b")))
    `(,(propertize
        (buttonize (pspmacs/shorten-it
                    buffer-string pspmacs/pspline-buffer-name-length)
                   #'pspmacs/pspline--toggle-read-only)
        'face `(,base ,box)
        'help-echo (format "Click to make buffer %s"
                           (if buffer-read-only "writable" "read-only")))
      " ")))

(defvar-local pspmacs/pspline-buffer-name
    '(:eval (pspmacs/pspline--buffer-name))
  "Buffer-name, process-state.

Customize face with `pspmacs/pspline-buffer-modified'.")

(defun pspmacs/pspline--buffer-process ()
  "Evaluated by `pspmacs/pspline-buffer-process'."
  (when-let
      (((pspmacs/pspline--display-segment 'pspmacs/pspline-buffer-process))
       (proc-string
        (pcase mode-line-process
          ((pred stringp) mode-line-process)
          ((pred symbolp)
           (when mode-line-process (symbol-name mode-line-process)))
          ((pred consp)
           (remq nil (mapconcat (lambda (x) (eval x)) mode-line-process))))))
    `(,(propertize
        (pspmacs/shorten-it proc-string pspmacs/pspline-buffer-name-length)
        'face 'pspmacs/pspline-buffer-process)
      " ")))

(defvar-local pspmacs/pspline-buffer-process
    '(:eval (pspmacs/pspline--buffer-process))
  "Buffer-process.")

(defun pspmacs/pspline--win-loc ()
  "Evaluated by `pspmacs/pspline-win-loc'."
  (when (pspmacs/pspline--display-segment 'pspmacs/pspline-win-loc)
    `(,(propertize
        (eval pspmacs/pspline-win-loc-format)
        'face (if (mode-line-window-selected-p) 'pspmacs/pspline-win-loc
                'mode-line-inactive))
      " ")))

(defvar-local pspmacs/pspline-win-loc '(:eval (pspmacs/pspline--win-loc))
  "Location of window in buffer.

Customize value with `pspmacs/pspline-win-loc-format'.
Customize face with `pspmacs/pspline-win-loc'.")

(defun pspmacs/pspline--cursor-pos ()
  "Evaluated by `pspmacs/pspline-cursor-pos'."
  (when (pspmacs/pspline--display-segment 'pspmacs/pspline-cursor-pos)
    `(,(propertize
        (eval pspmacs/pspline-cursor-pos-format)
        'face (if (mode-line-window-selected-p) 'pspmacs/pspline-cursor-pos
                'mode-line-inactive))
      " ")))

(defvar-local pspmacs/pspline-cursor-pos '(:eval (pspmacs/pspline--cursor-pos))
  "Cursor position indicator <row:col>.

Customize value with `pspmacs/pspline-cursor-pos-format'.
Customize face with `pspmacs/pspline-cursor-pos'.")

(defun pspmacs/pspline--cycle-evil-state (&optional back)
  "Cycle between evil states.

If BACK is non-nil, switch to previous state.
If current state is unidentifyable, switch to normal state by default.

Order of states:
- normal
- insert
- visual
- replace
- operator
- motion
- Emacs"
  (when (featurep 'evil)
    (if-let* ((evil-states-order
               '(evil-normal-state
                 evil-insert-state
                 evil-visual-state
                 evil-replace-state
                 evil-operator-state
                 evil-motion-state
                 evil-emacs-state))
              (current-position
               (cl-position (intern (format "evil-%s-state" evil-state))
                            evil-states-order)))
        (funcall (nth (mod (+ current-position (if back -1 1))
                           (length evil-states-order))
                      evil-states-order)))
    (evil-normal-state)))

(defun pspmacs/pspline--next-evil-state ()
  "Push `evil-state' in `pspmacs/pspline--cycle-evil-state'."
  (interactive)
  (pspmacs/pspline--cycle-evil-state))

(defun pspmacs/pspline--prev-evil-state ()
  "Pull back `evil-state' in `pspmacs/pspline--cycle-evil-state'."
  (interactive)
  (pspmacs/pspline--cycle-evil-state t))

(defvar pspmacs/pspline-evil-state-map
  (let ((map (make-sparse-keymap)))
    (define-key
     map [mode-line down-mouse-1] #'pspmacs/pspline--next-evil-state)
    (define-key
     map [mode-line down-mouse-3] #'pspmacs/pspline--prev-evil-state)
    map)
  "Mouse clicks on `evil-state' icon.")

(defun pspmacs/pspline--evil-state ()
  "Evaluated by `pspmacs/pspline-evil-state'."
  (when (and (pspmacs/pspline--display-segment 'pspmacs/pspline-evil-state)
             (featurep 'evil))
    `(,(propertize
        (eval pspmacs/pspline-evil-state-format)
        'face (if (not (mode-line-window-selected-p)) 'mode-line-inactive
                (cl-case evil-state
                  (normal 'pspmacs/pspline-evil-normal)
                  (insert 'pspmacs/pspline-evil-insert)
                  (visual 'pspmacs/pspline-evil-visual)
                  (replace 'pspmacs/pspline-evil-replace)
                  (operator 'pspmacs/pspline-evil-operator)
                  (motion 'pspmacs/pspline-evil-motion)
                  (emacs 'pspmacs/pspline-evil-emacs)
                  (_ 'pspmacs/pspline-evil-emacs)))
        'local-map pspmacs/pspline-evil-state-map
        'help-echo (symbol-name evil-state))
      " ")))

(defvar-local pspmacs/pspline-evil-state '(:eval (pspmacs/pspline--evil-state))

  "Evil state icon.
Customize faces with `pspmacs/pspline-evil-state-format',
`pspmacs/pspline-evil-normal',
`pspmacs/pspline-evil-insert',
`pspmacs/pspline-evil-visual',
`pspmacs/pspline-evil-replace',
`pspmacs/pspline-evil-operator',
`pspmacs/pspline-evil-motion',
`pspmacs/pspline-evil-emacs',
`pspmacs/pspline-evil-unknown'.")

(defun pspmacs/pspline--input-method ()
  "Evaluated by `pspmacs/pspline-input-method'."
  (when-let*
      (((pspmacs/pspline--display-segment 'pspmacs/pspline-input-method))
       (input-method (if (featurep 'evil) evil-input-method
                       current-input-method))
       ;; '(INPUT-METHOD LANGUAGE-ENV ACTIVATE-FUNC TITLE DESCRIPTION)
       (input-info (assoc input-method input-method-alist))
       (lang-env (nth 1 input-info))
       (title (nth 3 input-info))
       (indicator
        (or (cdr (assoc title pspmacs/pspline-input-method-replace-alist))
            title))
       (click-map (make-sparse-keymap)))
    (define-key click-map [mode-line down-mouse-1] #'toggle-input-method)
    (define-key click-map [mode-line down-mouse-3] #'set-input-method)

    `(,(propertize
        indicator
        'keymap click-map
        'help-echo (format "%s ❌" lang-env)
        'face (if (mode-line-window-selected-p) 'pspmacs/pspline-input-method
                'mode-line-inactive))
      " ")))

(defvar-local pspmacs/pspline-input-method
    '(:eval (pspmacs/pspline--input-method))
  "Cursor position indicator <row:col>.

Customize value with `pspmacs/pspline-input-method-replace-alist'.
Customize face with `pspmacs/pspline-input-method'.")

(defun pspmacs/pspline--info ()
  "Miscellaneous information as provided by `mode-line-misc-info'."
  (when (pspmacs/pspline--display-segment 'pspmacs/pspline-info)
    mode-line-misc-info))

(defvar-local pspmacs/pspline-info '(:eval (pspmacs/pspline--info))
  "Handle for miscellaneous information.")

(defun pspmacs/pspline--version-control ()
  "Evaluated by `pspmacs/pspline-version-control'."
  (when-let
      (((pspmacs/pspline--display-segment 'pspmacs/pspline-version-control))
       ((stringp vc-mode))
       (vc-spec
        (replace-regexp-in-string
         (format "^ %s[-:@]" (vc-backend buffer-file-name)) " " vc-mode)))
    (propertize
     (concat vc-spec " ")
     'face (if (not (mode-line-window-selected-p)) 'mode-line-inactive
             (pcase vc-spec
               (" main" 'pspmacs/pspline-vc-main)
               (" master" 'pspmacs/pspline-vc-main)
               (" release" 'pspmacs/pspline-vc-release)
               (_ 'pspmacs/pspline-vc-non-main))))))

(defvar-local pspmacs/pspline-version-control
  '(:eval (pspmacs/pspline--version-control))
  "Version control spec.

Customize faces with `pspmacs/pspline-vc-main',
`pspmacs/pspline-vc-non-main',
`pspmacs/pspline-vc-release'.")

(defun pspmacs/pspline--flymake-counter (type)
  "Plain `flymake--mode-line-counter' of TYPE without properties."
  (let ((count 0))
    (dolist (d (flymake-diagnostics))
      (when (= (flymake--severity type)
               (flymake--severity (flymake-diagnostic-type d)))
        (cl-incf count)))
    count))

(defvar pspmacs/pspline-flymake-map
  (let ((map (make-sparse-keymap)))
    (define-key map [mode-line down-mouse-1]
                #'flymake-show-buffer-diagnostics)
    (define-key map [mode-line down-mouse-3]
                #'flymake-show-project-diagnostics)
    map)
  "Keymap to display on Flymake indicator.")

(defun pspmacs/pspline--hint-part (type)
  "Derive hint part of flymake error TYPE."
  (let ((err (pspmacs/pspline--flymake-counter type)))
    (when (cl-plusp err)
      (propertize
       (format "%d " err)
       'face (intern (format "pspmacs/pspline-flymake-%s"
                             (string-trim (symbol-name type) ":")))
       'local-map pspmacs/pspline-flymake-map
       'help-echo (concat "mouse-1: buffer diagnostics"
                          "\n"
                          "mouse-3: project diagnostics")))))

(defun pspmacs/pspline--error-hints ()
  "Evaluated by `pspmacs/pspline-error-hints'."
  (when (and (pspmacs/pspline--display-segment 'pspmacs/pspline-error-hints)
             (featurep 'flymake))
    (let ((errr (pspmacs/pspline--hint-part :error))
          (wrng (pspmacs/pspline--hint-part :warning))
          (note (pspmacs/pspline--hint-part :note)))
      `(,(cond (errr pspmacs/pspline-flymake-error-icon)
               (wrng pspmacs/pspline-flymake-warning-icon)
               (note pspmacs/pspline-flymake-note-icon)
               (t pspmacs/pspline-flymake-good-icon))
        ,errr
        ,wrng
        ,note))))

(defvar-local pspmacs/pspline-error-hints
  '(:eval (pspmacs/pspline--error-hints))
  "Version control spec.  Customize faces with.")

(defun pspmacs/pspline--time ()
  "Evaluated by `pspmacs/pspline-time'."
  (when (pspmacs/pspline--display-segment 'pspmacs/pspline-time)
    `(,(propertize
        (format-time-string (eval pspmacs/pspline-time-string-format))
        'face 'pspmacs/pspline-time
        'help-echo (format-time-string "%c"))
      " ")))

(defvar-local pspmacs/pspline-time '(:eval (pspmacs/pspline--time))
  "Time segment.

Customize value with `pspmacs/pspline-time-string-format'.")

(defun pspmacs/pspline--battery-toggle-show-string (&optional _button)
    "Toggle display and help-text."
    (customize-set-variable
     'pspmacs/pspline--show-string
     (if (string= pspmacs/pspline--show-string "time") "percent" "time"))
    (force-mode-line-update t))

(defun pspmacs/pspline--battery-icon (perc)
  "Battery icon based on current battery percentage PERC."
  (cl-some (lambda (x) (when (> perc (car x)) (cdr x)))
           pspmacs/pspline-battery-icon-plist))

(defun pspmacs/pspline--battery ()
  "Evaluated by `pspmacs/pspline-battery'."
  (when-let* (((pspmacs/pspline--display-segment 'pspmacs/pspline-battery))
              ((not (equal
                     "N/A" (cdr (assq ?p (funcall battery-status-function))))))
              (battery-info (funcall battery-status-function))
              (hours-remain (concat (cdr (assq ?t battery-info)) "h"))
              (bat-perc (cdr (assq ?p battery-info)))
              (bat-perc-num (if (stringp bat-perc) (string-to-number bat-perc)
                              bat-perc))
              (bat-perc-string (format "%s%%" bat-perc-num))
              (bat-icon (pspmacs/pspline--battery-icon bat-perc-num))
              (bat-color (pspmacs/fill-color-cap bat-perc-num 1.0 t "#00ffff"))
              (bat-string
               (concat bat-icon
                       (when (string= pspmacs/pspline--show-string "time")
                         hours-remain (format "%s%%" bat-perc-string))))
              (tooltip-string
               (if (string= pspmacs/pspline--show-string "time")
                   bat-perc-string
                 hours-remain)))
    `(,(propertize
        (buttonize bat-string
                   #'pspmacs/pspline--battery-toggle-show-string)
        'face (if (string= (cdr (assq ?b battery-info)) "+")
                  `(:background ,bat-color :foreground ,"#000000")
                `(:foreground ,bat-color))
        'help-echo tooltip-string
        'mouse-face `(:foreground "#000000" :background ,bat-color))
      " ")))

(defvar-local pspmacs/pspline-battery '(:eval (pspmacs/pspline--battery))
  "Battery segment.

Customize value with `pspmacs/pspline-battery-icon-plist',
`pspmacs/pspline-battery-color-plist'.")

(dolist (seg pspmacs/pspline-segments-alist nil)
  (put (car seg) 'risky-local-variable t))

(defun pspmacs/pspline--display-segment (seg-symbol)
  "Whether SEG-SYMBOL should be displayed."
  (or (mode-line-window-selected-p)
      (cl-some (lambda (x)
                 (if (eq (car x) seg-symbol)
                     (plist-get (cdr x) :inactive)))
               pspmacs/pspline-segments-alist)))

(defun pspmacs/pspline--assert-all-the-icons ()
  "Assert that all-the-icons is installed.

Many default strings are derived from all-the-icons."
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
  "Buffer location in percentage or all.")

(defun pspmacs/pspline-generate ()
  "Generate format of pspline.

If current buffer is not focused,
only display segments meant for inactive buffer."
  (let* (left-segs right-segs)
    (dolist (seg pspmacs/pspline-segments-alist nil)
      (when (plist-get (cdr seg) :display)
        (let ((segment (eval (car seg))))
          (if (plist-get (cdr seg) :right) (push segment right-segs)
            (push segment left-segs)))))
    ;; Mode line format
    `("%e" mode-line-front-space ,@(nreverse left-segs)
      mode-line-format-right-align ,@(nreverse right-segs) " "
      mode-line-end-spaces)))

(defvar pspmacs/pspline-after-reset-hook nil
  "Normal hook run after enabling a theme.")

(defun pspmacs/pspline-run-after-reset-hook (&rest _args)
  "Run `pspmacs/pspline-after-reset-hook'."
  (run-hooks 'pspmacs/pspline-after-reset-hook))

(defun pspmacs/pspline-reset ()
  "Reset pspline as default mode-line.

When setting for first time, use `pspmacs/pspline-set-up'.
Hooks: `pspmacs/pspline-after-reset-hook'."
  (interactive)
  (let ((pspline-format (pspmacs/pspline-generate)))
    (setq-default mode-line-format pspline-format)
    (dolist (open-buff (buffer-list) nil)
      (with-current-buffer open-buff
        (setq mode-line-format (pspmacs/pspline-generate)))))
  (pspmacs/pspline--assert-all-the-icons))

(advice-add 'pspmacs/pspline-reset
            :after #'pspmacs/pspline-run-after-reset-hook)

;;;###autoload
(defun pspmacs/pspline-set-up ()
  "Set up pspline as mode-line.

Save current `mode-line-format' as `pspmacs/pspline--original-format'
To reset, consider `pspmacs/pspline-reset'"
  (interactive)
  (setq pspmacs/pspline--original-format mode-line-format)
  (pspmacs/pspline-reset))

(defun pspmacs/pspline-tear-down ()
  "Tear down pspline as mode-line.

Resetting to `pspmacs/pspline--original-format'"
  (interactive)
  (setq-default mode-line-format  pspmacs/pspline--original-format)
  (dolist (open-buff (buffer-list) nil)
    (with-current-buffer open-buff
      (setq mode-line-format pspmacs/pspline--original-format))))

(when (version< emacs-version "30")
  (defcustom mode-line-right-align-edge 'window
    "A back-port from master branch (version 30).

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
    "A back-port from master branch (version 30).

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
    (let* ((rest (cdr (memq 'mode-line-format-right-align mode-line-format)))
           (rest-str (format-mode-line `("" ,@rest)))
           (rest-width
            (progn (add-face-text-property
                    0 (length rest-str) 'mode-line t rest-str)
                   (string-pixel-width rest-str))))
      (propertize
       " "
       'display
       ;; The `right' spec doesn't work on TTY frames
       ;; when windows are split horizontally (bug#59620)
       (if (and (display-graphic-p)
                (not (eq mode-line-right-align-edge 'window)))
           `(space :align-to (- ,mode-line-right-align-edge (,rest-width)))
         `(space
           :align-to
           (,(- (window-pixel-width)
                (window-scroll-bar-width)
                (window-right-divider-width)
                (* (or (cdr (window-margins)) 1) (frame-char-width))
                ;; Manually account for value of `mode-line-right-align-edge'
                ;; even when display is non-graphical
                (pcase mode-line-right-align-edge
                  ('right-margin (or (cdr (window-margins)) 0))
                  ;; what here?
                  ('right-fringe (or (cadr (window-fringes)) 0))
                  (_ 0))
                rest-width)))))))

  (defvar mode-line-format-right-align
    '(:eval (mode--line-format-right-align))
    "A back-port from master branch (version 30).

Mode line construct to right align all following constructs.")
  ;;;###autoload
  (put 'mode-line-format-right-align 'risky-local-variable t))

(provide 'pspmacs/pspline)
;;; pspline.el ends here

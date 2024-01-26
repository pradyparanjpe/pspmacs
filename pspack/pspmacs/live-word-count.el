;;; live-word-count.el --- Display live word counts in mode-line -*- lexical-binding: t; -*-

;; Copyright © 2024 Pradyumna Paranjape.

;; Author: Pradyumna Paranjape <pradyparanjpe@rediffmail.com>
;; URL: https://www.gitlab.com/pradyparanjpe/pspmacs
;; Version: 0.0.1
;; Package-Requires: ((emacs "29.1"))

;; This file is NOT part of GNU Emacs.
;; This file is a part of PSPMacs

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; Count text words (excluding code, properties, comments) on the fly.
;; Set target for the buffer, display as absolute words, fraction of target.
;; Restrict word count to region if selected, show as fraction of total.
;; Minor mode: provides a mode-line eval string.
;; Customize using customization group `live-word-count'.

;;; Code:

(require 'pspmacs/common)

(defgroup live-word-count nil
  "Colored display segment of word counts on mode line."
  :group 'convenience
  :group 'display
  :prefix "live-word-count")

(defcustom live-wc-unbind-modes
  '(prog-mode dired-mode special-mode)
  "Major modes and their derivatives, in which, word-count is not displayed.

 This list overrides `live-wc-bind-modes'."
  :type '(repeat (symbol :tag "Mode in which, word count is inactivated"))
  :group 'live-word-count)

(defcustom live-wc-ignore-if
  '((:ignore (lambda () (nth 4 (syntax-ppss))) :desc "comment (by property)")
    (:ignore (lambda () (looking-at (format " *%s" comment-start-skip)))
             :desc "comment (by marker)")
    (:ignore org-at-comment-p :desc "org comment")
    (:ignore org-at-keyword-p :desc "org keyword")
    (:ignore org-at-table-p :desc "org table")
    (:ignore org-at-TBLFM-p :desc "org table formula")
    (:ignore org-at-table.el-p :desc "table.el")
    (:ignore org-at-heading-p :desc "org heading")
    (:ignore org-at-property-p :desc "org property")
    (:ignore org-at-drawer-p :desc "org drawer")
    (:ignore org-at-property-drawer-p :desc "property drawer's first line")
    (:ignore pspmacs/in-org-block-p :desc "any org block"))

  "Where any of the functions returns non-nil, ignore the line."
  :type '(repeat (choice (function :tag "predicate")
                         (plist ((const :ignore) (function :tag "predicate"))
                                ((const :desc) (string :tag "description")))))
  :group 'live-word-count)

(defcustom live-wc-max-buffer-size
  15360
  "Maximum size of buffer beyond which, word count is inactive."
  :type 'number
  :group 'live-word-count)

(defvar-local live-wc-target nil
  "Targetted number of (text) words to write in buffer.

If non-nil, `live-wc-do-count' will use this as the \=TARGET\=.
Value \=0\= is interpreted as nil.
If the value is negative, it is interpreted as \=CAP\= (upper limit).")

(defvar-local live-wc-fraction t
  "If non-nil and if possible, show value as a fraction.

If region is selected, display fraction of all the text.
Else, display fraction of `live-wc-target' if set.
Else, fallback to absolute.")

(defvar live-wc-map
  (let ((map (make-sparse-keymap)))
    (define-key map [mode-line down-mouse-1] #'live-wc--set-target)
    (define-key map [mode-line down-mouse-3] #'live-wc--toggle-format)
    map)
  "Keymap to display on word-count indicator.")

(defvar-local live-wc-mem
    nil
  "Memory of displayed value for reuse (esp. while nothing changes)")

(defun live-wc--set-target ()
  "Set value for `live-wc-target'."
  (interactive)
  (let ((wc-target (read-number
                    "Set word count target:\t"
                    (if live-wc-target (- live-wc-target) 0))))
    (setq-local live-wc-target (if (= 0 wc-target) nil wc-target))))

(defun live-wc--toggle-format ()
  "Toggle `live-wc-fraction'."
  (interactive)
  (setq-local live-wc-fraction (not live-wc-fraction)))

(defun live-wc--count-text-words (&optional complete-buffer)
  "Return a p-list of number of buffers in the buffer.

If a region is selected and COMPLETE-BUFFER is nil, restrict to that region."
  (interactive)
  (let* ((num-lines 0)
         (num-bytes 0)
         (num-words 0)
         (restrict (when (and (not complete-buffer) (use-region-p)) t))
         (reg-beg (if restrict (region-beginning) (point-min)))
         (reg-end (if restrict (region-end) (point-max))))
    (save-excursion
      (goto-char reg-beg)
      (while (< (point) reg-end)
        ;; (beginning-of-line)
        (when (cl-notany
               (lambda (x) (funcall (or (plist-get x :ignore) x)))
               live-wc-ignore-if)
          (let ((line-beg (line-beginning-position))
                (line-end (min (line-end-position) reg-end)))
            (cl-incf num-lines)
            (cl-incf num-bytes (- line-end line-beg))
            (cl-incf num-words (count-words line-beg line-end))))
        (forward-line 1)))
    `((lines ,num-lines) (bytes ,num-bytes) (words ,num-words))))

(defun live-wc--color (disp-text &optional swap)
  "Translate disp-text into color.

DISP-TEXT is the displayed text, used to decide color.
Non-nil SWAP swaps :background and :foreground."
  (if (not (mode-line-window-selected-p))
      'mode-line-inactive
    (if (not (string-match "%" disp-text))
        'mode-line-active  ;; simple absolute count
      (let* ((disp-num
              (string-to-number
               (progn (string-match "\\([0-9]+\\.?[0-9]*\\)" disp-text)
                      (match-string 1 disp-text))))
             (disp-color (pspmacs/fill-color-cap
                          (/ disp-num 100)
                          1.0
                          (not swap))))
        (if (and (> disp-num 100) swap)
            `(:foreground ,(invert-color-hex disp-color)
                          :background ,disp-color)
          `(:foreground ,disp-color))))))

(defun live-wc-do-count ()
  "Evaluated by `live-wc-eval-str'.

 If `live-wc--target' is non nil, display as percent of target."
  (when (or (use-region-p)
            (< (buffer-size) live-wc-max-buffer-size))
    `(,(let* ((counts (live-wc--count-text-words))
              (num-words (nth 0 (alist-get 'words counts)))
              (hint (mapconcat (lambda (x)
                                 (format "%d %s\n" (nth 1 x) (car x)))
                               counts))
              (target (when (and live-wc-target (/= live-wc-target 0))
                        (abs live-wc-target)))
              (disp-text
               (cond
                ((not (and (or (use-region-p) live-wc-target)
                           live-wc-fraction))
                 (number-to-string num-words))
                ((use-region-p)
                 (format "%2.2f%%%%"
                         (* 100
                            (/ (float num-words)
                               (nth 0 (alist-get
                                       'words
                                       (live-wc--count-text-words t)))))))
                (t (format "%2.2f%%%%"
                           (* 100 (/ (float num-words) target))))))
              (disp-face (live-wc--color
                          disp-text
                          (when (and target (> 0 live-wc-target)) t)))
              (mem (propertize (format "¶:%s" disp-text)
                               'local-map live-wc-map
                               'face disp-face
                               'mouse-face disp-face
                               'help-echo
                               (concat hint (when target
                                              (format "of %d" target))))))
         (setq-local live-wc-mem mem)
         mem)
      " ")))

(defvar-local live-wc-eval-str
    nil
  "Live word count in mode-line.

Customize-Save-Variable value with `live-wc-max-buffer-size',
`live-wc-unbind-modes'")

;;;###autoload
(define-minor-mode live-word-count-mode
  "Toggle live-word-count-mode.

When live-word-count-mode is ON, `live-wc-eval-str'
displays current wc value, nil otherwise."
  :lighter nil
  (setq-local
   live-wc-eval-str
   (when (and live-word-count-mode
              (cl-notany (lambda (x) (derived-mode-p x)) live-wc-unbind-modes))
     '(:eval (progn (when (buffer-modified-p)
                      (setq-local live-wc-mem nil))
                    (or live-wc-mem (live-wc-do-count)))))))

(require 'pspmacs/pspline)
(defvar-local pspmacs/pspline-word-count
    '(:eval (when (pspmacs/pspline--display-segment
                   'pspmacs/pspline-word-count)
              live-wc-eval-str))
  "Display live word count from `live-word-count-mode'")

;;;###autoload
(defun live-wc-set-pspline-seg (&optional pos inactive)
  "Insert segment in `pspmacs/pspline-segments-plist'

If POS is non-nil, insert segment at that position (x2 for p-list)
If POS > number of existing segments, or nil, insert at end.
If INACTIVE is non-nil, show segment even when buffer is inactive"
  (let ((insert-at (min (or pos most-positive-fixnum)
                        (length pspmacs/pspline-segments-plist))))
    (unless (string= (car (nth insert-at pspmacs/pspline-segments-plist))
                     'pspmacs/pspline-word-count)
      (customize-set-variable
       'pspmacs/pspline-segments-plist
       (append (subseq pspmacs/pspline-segments-plist 0 insert-at)
               '((pspmacs/pspline-word-count
                  . (:display t :right nil :inactive inactive)))
               (subseq pspmacs/pspline-segments-plist insert-at)))
      (pspmacs/pspline-reset))))

(put 'live-wc-target 'safe-local-variable #'numberp)
(put 'live-wc-fraction 'safe-local-variable #'booleanp)
(put 'live-wc-mem 'risky-local-variable t)
(put 'live-wc-eval-str 'risky-local-variable t)
(put 'pspmacs/pspline-word-count 'risky-local-variable t)

(provide 'pspmacs/live-word-count)
;;; live-word-count.el ends here
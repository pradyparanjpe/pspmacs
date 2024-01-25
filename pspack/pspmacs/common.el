;;; common.el --- common objects -*- lexical-binding: t; -*-
;;; Commentary:
;;
;; COMMON ModeLine for Emacs
;;
;;; Code:

(defcustom pspmacs/long-str-ellipses
  "…"
  "Character(s) that indicates that the name was trimmed"
  :type '(string)
  :group 'pspmacs)

(defun pspmacs/shorten-it (long-str &optional shorten-to ellipses)
  "Shorten string

  If LONG-STR is longer than SHORTEN-TO, then, shorten it by inserting ELLIPSES
LONG-STR may be a string (preferred), symbol or sequence of allowed types.
When sequence, the function is applied to each element.

Default shorten-to is `fill-column'.
Default ellipses are customized using `pspmacs/long-str-ellipses'"
  (let ((ellipses (or ellipses pspmacs/long-str-ellipses))
        (shorten-to (or shorten-to fill-column)))
    (cond ((stringp long-str)
           (let ((str-mid (/ (length long-str) 2))
                 (str-cut (ceiling (/ (- (+ (length long-str) (length ellipses))
                                         shorten-to)
                                      2))))
             (if (cl-plusp str-cut)
                 (concat (substring long-str 0 (- str-mid str-cut))
                         ellipses
                         (substring long-str (+ str-mid str-cut)))
               long-str)))

          ;; 'symbol to "string"
          ((symbolp long-str)
           (pspmacs/shorten-it (symbol-name long-str) shorten-to ellipses))

          ;; Apply to each element in sequence (possible deep stack)
          ;; When using deep nested sequences,
          ;;     the /=user/= must worry about deep stack.
          ((sequencep long-str)
           (mapcar (lambda (x) (pspmacs/shorten-it x shorten-to ellipses))
                   long-str)))))

(defcustom pspmacs/fill-color-overflow-color "#ff00ff"
  "Color of indicator when line overflows `fill-column'"
  :type 'color
  :group 'pspmacs/fill-color)

(defun pspmacs/invert-color-hex (hex)
  "Return a color hex-string #(0xff-R) (0xff-G) (0xff-B) from HEX."
  (apply #'color-rgb-to-hex `(,@(pspmacs/invert-color (color-name-to-rgb hex)) 2)))

(defun pspmacs/invert-color (rgb)
  "Return a list of (1-R 1-G 1-B) from RGB."
  (mapcar (lambda (p) (- 1 p)) rgb))

(defun pspmacs/fill-color-cap (frac &optional bright invert overflow)
  "Color based on filled capacity fraction FRAC.

\=0\= is empty (good = cyan), \=1\= is filled (bad = red)
Fraction of brightness is provided through BRIGHT, else assumed as 1.0
FRAC > 1.0 is interpreted as *overfilled* and returns
BRIGHT-scaled `pspmacs/fill-color-overflow-color')
If OVERFLOW is non-nil, return that color for overflow instead.
If INVERT is not-nil, return inverted pallet, i.e.
\=0\= is empty (bad = red), \=1\= is filled (good = cyan)"
  (let* ((bright (max 0 (min (or bright 1) 1)))
         (frac (if invert (- 1.0 frac) frac))
         (rgb (if (or (> frac 1.0) (< frac 0.0))
              (color-name-to-rgb
               (or overflow pspmacs/fill-color-overflow-color))
              (let* ((red (min 1.0 (* 2.0 frac)))
                     (green (min 1.0 (* 2.0 (- 1.0 frac))))
                     (blue (max 0 (- 1.0 (* 10 frac)))))
                `(,red ,green ,blue))))
    (col-vals (mapcar (lambda (x) (* bright x)) rgb)))
  (apply #'color-rgb-to-hex `(,@col-vals 2))))

(defun pspmacs/in-org-block-p ()
  "Return non-nil if inside any block.

Does not necessitate Org \=Special\= Block.

This function is heavily adapted from `org-between-regexps-p'.
Gratefully copied from https://scripter.co/splitting-an-org-block-into-two/
Under the function named \=modi/org-in-any-block-p\=.
"
  (save-match-data
    (let ((pos (point))
          (case-fold-search t)
          (block-begin-re "^[[:blank:]]*#\\+begin_\\(?1:.+?\\)\\(?: .*\\)*$")
          (limit-up (save-excursion (outline-previous-heading)))
          (limit-down (save-excursion (outline-next-heading)))
          beg end)
      (save-excursion
        ;; Point is on a block when on BLOCK-BEGIN-RE or if
        ;; BLOCK-BEGIN-RE can be found before it...
        (and (or (org-in-regexp block-begin-re)
                 (re-search-backward block-begin-re limit-up :noerror))
             (setq beg (match-beginning 0))
             ;; ... and BLOCK-END-RE after it...
             (let ((block-end-re (concat "^[[:blank:]]*#\\+end_"
                                         (match-string-no-properties 1)
                                         "\\( .*\\)*$")))
               (goto-char (match-end 0))
               (re-search-forward block-end-re limit-down :noerror))
             (> (setq end (match-end 0)) pos)
        ;; ... without another BLOCK-BEGIN-RE in-between.
        (goto-char (match-beginning 0))
        (not (re-search-backward block-begin-re (1+ beg) :noerror))
        ;; Return value.
        (cons beg end))))))

(provide 'pspmacs/common)
;;; common.el ends there

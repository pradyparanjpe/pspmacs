;;; common.el --- common objects -*- lexical-binding: t; -*-
;;; Commentary:
;;
;; COMMON ModeLine for Emacs
;;
;;; Code:
(require 'cl-lib)
(require 'color)

(defcustom pspmacs/long-str-ellipses "…"
  "Character(s) that indicates that the name was trimmed."
  :type '(string)
  :group 'pspmacs)

(defun pspmacs/shorten-it (long-str &optional shorten-to ellipses)
  "Shorten string.

If LONG-STR is longer than SHORTEN-TO, then, shorten it by inserting ELLIPSES
LONG-STR may be a string (preferred), symbol or sequence of allowed types.
When sequence, the function is applied to each element.

Default shorten-to is `fill-column'.
Default ellipses are customized using `pspmacs/long-str-ellipses'."
  (let ((ellipses (or ellipses pspmacs/long-str-ellipses))
        (shorten-to (or shorten-to fill-column)))
    (cond ((stringp long-str)
           (if-let* ((long-str (abbreviate-file-name long-str))
                     (str-mid (/ (length long-str) 2))
                     (str-cut
                      (ceiling (/ (- (+ (length long-str) (length ellipses))
                                     shorten-to)
                                  2)))
                     ((cl-minusp str-cut)))
               long-str
             (concat (substring long-str 0 (- str-mid str-cut))
                     ellipses
                     (substring long-str (+ str-mid str-cut)))))

          ((symbolp long-str)
           (pspmacs/shorten-it (symbol-name long-str) shorten-to ellipses))

          ;; Apply to each element in sequence (possible deep stack)
          ;; When using deep nested sequences,
          ;;     the \\='user\\=' must worry about stack depth.
          ((sequencep long-str)
           (mapcar (lambda (x) (pspmacs/shorten-it x shorten-to ellipses))
                   long-str)))))

(defcustom pspmacs/fill-color-overflow-color "#ff00ff"
  "Color of indicator when line overflows `fill-column'."
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

\\='0\\=' is empty (good = cyan), \\='1\\=' is filled (bad = red)
Fraction of brightness is provided through BRIGHT, else assumed as 1.0
FRAC > 1.0 is interpreted as *overfilled* and returns
BRIGHT-scaled `pspmacs/fill-color-overflow-color')
If OVERFLOW is non-nil, return that color for overflow instead.
If INVERT is not-nil, return inverted pallet, i.e.
\\='0\\=' is empty (bad = red), \\='1\\=' is filled (good = cyan)"
  (let ((frac (if invert (- 1.0 frac) frac)))
    (apply
     #'color-rgb-to-hex
     `(,@(mapcar
          (lambda (x) (* (max 0 (min (or bright 1) 1)) x))
          (if (or (< frac 0.0) (> frac 1.0))
              (color-name-to-rgb
               (or overflow pspmacs/fill-color-overflow-color))
            `(,(min 1.0 (* 2.0 frac))  ; red
              ,(min 1.0 (* 2.0 (- 1.0 frac)))  ; green
              ,(max 0 (- 1.0 (* 10 frac))))))  ; blue
       2))))

(provide 'pspmacs/common)
;;; common.el ends here

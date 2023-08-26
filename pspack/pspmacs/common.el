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

(provide 'pspmacs/common)
;;; common.el ends there

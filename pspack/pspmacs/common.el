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

(defun pspmacs/shorten-it (long-str shorten-to &optional ellipses)
  "Shorten string

  If LONG-STR is longer than SHORTEN-TO, then, shorten it by inserting ELLIPSES"
  (let ((ellipses (or ellipses pspmacs/long-str-ellipses)))
    (cond ((stringp long-str)
           (let* ((buffer-mid (/ (length long-str) 2))
                  (buffer-cut
                   (+ (- buffer-mid (/ shorten-to 2))
                      (/ (length ellipses) 2) 1)))
             (if (cl-plusp buffer-cut)
                 (concat (substring long-str 0 (- buffer-mid buffer-cut))
                         ellipses
                         (substring long-str (+ buffer-mid buffer-cut)))
               long-str)))
          ((sequencep long-str)
           (mapcar (lambda (x)
                     (pspmacs/shorten-it x shorten-to ellipses))
                   long-str))
          ((symbolp long-str)
           (pspmacs/shorten-it (symbol-name long-str) shorten-to ellipses)))))

(provide 'pspmacs/common)
;;; common.el ends there

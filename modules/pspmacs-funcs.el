;;; pspmacs-funcs.el --- common miscellaneneous pspmacs functions  -*- lexical-binding: t; -*-

;; Copyright (C) 2023  Pradyumna Swanand Paranjape

;; Author: Pradyumna Swanand Paranjape <pradyparanjpe@rediffmail.com>
;; Keywords: help, languages

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU Lesser General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU Lesser General Public License for more details.

;; You should have received a copy of the GNU Lesser General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;;; Code:

(defun pspmacs/kill-other-buffers ()
  "Kill all other buffers."
  (interactive)
  (mapc 'kill-buffer (delq (current-buffer) (buffer-list))))

(defun pspmacs/destroy-buffer-and-window (&optional target-buffer)
  "Destroy window and buffer after some process is done

If TARGET-BUFFER is supplied, it and its window is destroyed.
Else, current buffer and window is destroyed.
If window is the only window, it is spared"
  (let* ((used-buffer (or target-buffer (current-buffer)))
         (used-window (get-buffer-window used-buffer)))
    (when (not (one-window-p))
      (delete-window used-window))
    (kill-buffer used-buffer)))

(defun pspmacs/extend-list (list-var elements)
  "Iterative form of ‘add-to-list’.

Return value is the new value of LIST-VAR"
  (unless (consp elements)
    (error "ELEMENTS must be list"))
  (dolist (elem elements)
    (add-to-list list-var elem))
  (symbol-value list-var))

(setq wl-copy-process nil)
(defun wl-copy (text)
  (setq wl-copy-process (make-process :name "wl-copy"
                  :buffer nil
                  :command '("wl-copy" "-f" "-n")
                  :connection-type 'pipe))
  (process-send-string wl-copy-process text)
  (process-send-eof wl-copy-process))
(defun wl-paste ()
  (if (and wl-copy-process (process-live-p wl-copy-process))
  nil ; should return nil if we're the current paste owner
    (shell-command-to-string "wl-paste -n | tr -d \r")))

(when (string-collate-equalp (getenv "XDG_SESSION_TYPE") "WAYLAND" nil t)
  (setq interprogram-cut-function 'wl-copy)
  (setq interprogram-paste-function 'wl-paste))

(defun pspmacs/yank-file-name ()
  "Yank file-name to clipboard"
  (interactive)
  (kill-new buffer-file-name))

(defun pspmacs/project-to-publish-alist
    (org-root html-root org-templates)
  "Set root locations for source ORG-ROOT and target HTML-ROOT

to publish orgmode files to html."
  (interactive
   (let (org-root html-root org-templates)
     (setq org-root (read-directory-name
             "ORG Directory:\t"
             nil default-directory
             ".*" nil))
     (setq html-root (read-directory-name
          "HTML Directory:\t"
          (expand-file-name "../html" org-root) nil
          ".*" nil))
     (setq org-templates (read-directory-name
              "Templates Directory:\t"
              (expand-file-name "templates"
                        pspmacs/org-template-path)
              nil ".*" nil))
     (list org-root html-root org-templates)))

  (catch 'pspmacs/mk-tag
    (unless (file-directory-p html-root)
  (if (yes-or-no-p (format "%s doesn't exist. Create? " html-root))
      (make-directory html-root t)
    (throw 'pspmacs/mk-tag nil)))
    (setq org-publish-project-alist
      (list
       (list "org-notes"
         :base-directory org-root
         :base-extension "org"
         :publishing-directory html-root
         :recursive t
         :publishing-function 'org-html-publish-to-html
         :headline-levels 4
         :auto-preamble t)
       (list "org-static"
         :base-directory org-root
         :base-extension
         "css\\|js\\|png\\|jpg\\|gif\\|pdf\\|mp3\\|ogg\\|swf"
         :publishing-directory html-root
         :recursive t
         :publishing-function 'org-publish-attachment)
       (list "org-templates"
         :base-directory org-templates
         :base-extension
         "css\\|js\\|png\\|jpg\\|gif\\|pdf\\|mp3\\|ogg\\|swf"
         :publishing-directory html-root
         :recursive t
         :publishing-function 'org-publish-attachment)
       (list "org" :components
         '("org-notes" "org-static" "org-templates"))))))

(setq pspmacs/pretty-alist
      '(("code" . (("\\n" . ?⏎)
                   ("\\t" . ?↹)
                   (">=" . ?≥)
                   ("<=" . ?≤)
                   ("!=" . ?≠)
                   ("==" . ?≅)))
        ("lisp" . (("lambda" . ?λ)))
        ("org" . (("#+setupfile" . ?🛒)
                  ("#+author" . ?🖋)
                  ("#+begin_src" . ?)
                  ("#+end_src" . ?⏎)
                  ("#+email" . ?✉)
                  ("#+language" . ?🗣)
                  ("#+options" . ?🔘)
                  ("#+property" . ?⚙)
                  ("#+results" . ?📜)
                  ("#+startup" . ?)
                  ("#+html_head" . ?)
                  ("#+title" . ?§)
                  ("tangle" . ?🔗)
                  ("[x]" . ?✔)
                  ("[ ]" . ?❌)
                  ("[-]" . ?⏳)))
        ("python" . (("and" . ?∩)
                     ("or" . ?∪)
                     ("->" . ?⇒)))))

(defun pspmacs/mode-prettify
    (sub-modes)
  "Apply pretiffy mode alist according to active-mode"
  (progn
    (setq prettify-symbols-alist
          (mapcan (lambda (x)
                    (list x `(,(upcase (car x)) . ,(cdr x))))
                  (apply #'append
                         (mapcar (lambda (y)
                                   (cdr (assoc y pspmacs/pretty-alist)))
                                 sub-modes))))
    (prettify-symbols-mode)))

(pspmacs/load-inherit)
(provide 'pspmacs-funcs)
;;; pspmacs-funcs.el ends here

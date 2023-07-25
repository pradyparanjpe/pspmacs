;;; startpage.el --- startpage modeline -*- lexical-binding: t; -*-
;;; Commentary:
;;
;; STARTPAGE file-system awareness for pspmacs
;;
;;; Code:
(defgroup startpage nil
  "STARTPAGE for pspmacs."
  :group 'pspmacs)

(use-package recentf
  :commands (recentf-list)
  :demand t
  :config
  (recentf-mode 1))

(defcustom pspmacs/startpage-buffer-name "*StartPage*"
  "Name of startpage buffer"
  :type 'string
  :group 'startpage)

(defcustom pspmacs/startpage-recentf-num 5
  "number of recent files to link"
  :type 'integer
  :group 'startpage)

(defcustom pspmacs/startpage-projects-num 3
  "number of projects to link"
  :type 'integer
  :group 'startpage)

(defcustom pspmacs/startpage-banner-image
  (expand-file-name "data/banners/Tux.svg" user-emacs-directory)
  "Banner Image"
  :type '(file :must-match t)
  :group 'startpage)

(defcustom pspmacs/startpage-banner-scale-width 1.5
  "Width of `pspmacs/startpage-banner-image' in pixels
 is set as this factor time frame width in columns"
  :type 'integer
  :group 'startpage)

(defcustom pspmacs/startpage-banner-ascii
  (expand-file-name "data/banners/2.txt" user-emacs-directory)
  "Banner Image"
  :type '(file :must-match t)
  :group 'startpage)

(defcustom pspmacs/startpage-url-links
  '(("Documentation" . "https://pradyparanjpe.gitlab.io/pspmacs/index.html")
    ("Repository" . "https://gitlab.com/pradyparanjpe/pspmacs"))
  "Details of url links to display"
  :type '(repeat (cons (string :tag "display text")
                       (string :tag "url")))
  :group 'startpage)

(defcustom pspmacs/startpage-banner-face-props
  '((:foreground "#9f8f4f"))
  "Links face properties"
  :type 'plist
  :group 'startpage)

(defcustom pspmacs/startpage-url-links-face-props
  '((:foreground "#6fafcf") (:underline nil))
  "URL links face properties"
  :type 'plist
  :group 'startpage)

(defcustom pspmacs/startpage-block-title-face-props
  '((:foreground "#ff007f")
    (bold))
  "Quicklink block Title Face"
  :type 'plist
  :group 'startpage)

(defcustom pspmacs/startpage-block-link-face-props
  '((:foreground "#af9fa7") (:underline nil))
  "Quicklink block Link item faces"
  :type 'plist
  :group 'startpage)

(defcustom pspmacs/startpage-load-time-face-props
  '((:foreground "#bfdfff")
    (:background "#002040")
    italic)
  "Load-time information face"
  :type 'plist
  :group 'startpage)

(defvar pspmacs/startpage-recent-files-point
  (point-min)
  "Point to recent files")

(defvar pspmacs/startpage-projects-point
  (point-min)
  "Point to projects")

(defun pspmacs/startpage--shorten-path (filepath)
  "Shorten FILEPATH replacing home-directory by ~"
  (replace-regexp-in-string (getenv "HOME") "~" filepath))

(defun pspmacs/startpage--ascii-banner ()
    "Put ASCII Banner for non-graphic frames"
    (let* ((banner (split-string
                    (f-read pspmacs/startpage-banner-ascii) "\n"))
           (banner-width (length (nth 0 banner)))
           (pad-string (pspmacs/startpage--center-pad-string banner-width))
           (render-banner
            (mapcan
             (lambda (line)
               (progn
                 (add-face-text-property
                  0 (length line) pspmacs/startpage-banner-face-props t line)
                 `(,pad-string ,line)))
             banner)))
      (when (> (window-width) banner-width)
        (eval `(insert ,@render-banner)))))

(defun pspmacs/startpage--graphic-banner ()
  "Put Image Banner for graphic frames"
  (let* ((width (round (* pspmacs/startpage-banner-scale-width
                          (window-width))))
         (banner (create-image
                  pspmacs/startpage-banner-image
                  nil nil :width width))
         (pad-string (pspmacs/startpage--center-pad-string
                      (car (image-size banner)))))
    (insert pad-string)
    (insert-image banner)))

(defun pspmacs/startpage--evil-bind-jumps ()
  "Bind following keys (evil):
r: RECENT point
p: PROJECT point
R: `pspmacs/startpage-refresh'"
  (keymap-set evil-normal-state-local-map
              (kbd "r") (lambda () (interactive)
                          (goto-char pspmacs/startpage-recent-files-point)))
  (keymap-set evil-normal-state-local-map
              (kbd "p") (lambda () (interactive)
                          (goto-char pspmacs/startpage-projects-point)))
  (keymap-set evil-normal-state-local-map
              (kbd "R") 'pspmacs/startpage-refresh))

(defun pspmacs/startpage--native-bind-jumps ()
    "Bind following keys (native):
r: RECENT point
p: PROJECT point
R: `pspmacs/startpage-refresh'"
    (use-local-map (copy-keymap text-mode-map))
    (local-set-key (kbd "r")
                   (lambda () (interactive)
                     (goto-char pspmacs/startpage-recent-files-point)))
    (local-set-key (kbd "p")
                   (lambda () (interactive)
                     (goto-char pspmacs/startpage-projects-point)))
    (local-set-key (kbd "R") 'pspmacs/startpage-refresh))

(defun pspmacs/startpage--center-pad-string (display-width)
  "Left padding to center text if DISPLAY-WIDTH size"
  (concat "\n"
          (make-string
           (round (/ (max 0 (- (window-width) display-width)) 2))
           ? )))

(defun pspmacs/startpage--put-block (block-list &optional num block-title)
  "Place center-aligned block of links.

Links to files in BLOCK-LIST are enlisted in the block.
If NUM is non-zero, only NUM elements from block list are inserted.
If BLOCK-TITLE is non-nil, it is placed as a heading to the block.
Returns point to BLOCK-TITLE"
  (let* ((num (if num num (length block-list)))
         (items (subseq block-list 0 num))
         (max-len (apply #'max (mapcar (lambda (fname) (length fname)) items)))
         (pad-string (pspmacs/startpage--center-pad-string max-len))
         (block-point nil)
         (recent-links
          (mapcan
           (lambda (fname)
             (let ((button
                    (buttonize
                     (pspmacs/startpage--shorten-path fname)
                     (lambda (_button) (find-file fname)))))
               (add-face-text-property
                0 (length button)
                pspmacs/startpage-block-link-face-props
                nil button)
               `(,pad-string ,button)))
           items)))
    (add-face-text-property
     0 (length block-title)
     pspmacs/startpage-block-title-face-props t block-title)
    (insert (string-trim-right pad-string "  $") block-title)
    (setq block-point (point))
    (eval `(insert ,@recent-links))
    block-point))

(defun pspmacs/startpage-put-recentf ()
  "Place a block of recentf files

customize number `pspmacs/startpage-recentf-num'"
  (customize-set-variable 'pspmacs/startpage-recent-files-point
                          (pspmacs/startpage--put-block
                           recentf-list
                           pspmacs/startpage-recentf-num
                           "(r) Recent Files")))

(defun pspmacs/startpage-put-projects ()
  "Place a block of known projects

customize number `pspmacs/startpage-projects-num'"
  (customize-set-variable 'pspmacs/startpage-projects-point
                          (pspmacs/startpage--put-block
                           (project-known-project-roots)
                           pspmacs/startpage-projects-num
                           "(p) Projects")))

(defun pspmacs/startpage-put-banner ()
  "Place center-aligned banner in current buffer.

If `display-graphic-p', use `pspmacs/startpage-banner-image'
else, use `pspmacs/startpage-banner-ascii'"
  (if (display-graphic-p)
      (pspmacs/startpage--graphic-banner)
    (pspmacs/startpage--ascii-banner)))

  (defun pspmacs/startpage-bind-jumps ()
    "Bind jumps to locations RECENT and PROJECT in buffer."
    (if evil-state
        (pspmacs/startpage--evil-bind-jumps)
      (pspmacs/startpage--native-bind-jumps)))

  (defun pspmacs/startpage-put-load-time ()
    "Load time information"
    (let* ((load-string
            (format
             (emacs-init-time
              "Loaded %%d packages in %3.2f seconds")
             (length package-activated-list)))
           (pad-string (pspmacs/startpage--center-pad-string
                        (length load-string))))
      (add-face-text-property
       0 (length load-string)
       pspmacs/startpage-load-time-face-props t load-string)
      (insert "\n" pad-string load-string)))

(defun pspmacs/startpage-put-url-links ()
  "Place pspmacs links"
  (let* ((num-letters (apply '+ (mapcar (lambda (x)
                                          (+ (length (car x)) 2))
                                        pspmacs/startpage-url-links)))
         (spacer (make-string (/ (- (window-width) num-letters)
                                 (1+ (length pspmacs/startpage-url-links)))
                              ? ))
         (links-text (mapcar (lambda (item)
                               (let ((button (buttonize (car item)
                                           (lambda (_button)
                                             (browse-url (cdr item))))))
                                 (add-face-text-property
                                  0 (length button)
                                  pspmacs/startpage-url-links-face-props
                                  nil button)
                                 (concat
                                  spacer
                                  "● "
                                  button)))
                             pspmacs/startpage-url-links)))
    (eval `(insert ,@links-text))))

(defun pspmacs/startpage-refresh ()
  "Refresh start-page

Returns buffer handle"
  (interactive)
  (let ((startpage-buffer (get-buffer-create pspmacs/startpage-buffer-name)))
    (with-current-buffer startpage-buffer
      (special-mode)
      (when (featurep 'whitespace) (whitespace-mode -1))
      (when (featurep 'linum) (linum-mode -1))
      (when (featurep 'display-line-numbers) (display-line-numbers-mode -1))
      (when (featurep 'page-break-lines) (page-break-lines-mode 1))
      (when (featurep 'linum) ())
      (read-only-mode -1)
      (erase-buffer)
      (save-excursion
        (pspmacs/startpage-put-banner)
        (pspmacs/startpage-put-load-time)
        (insert "\n\n")
        (pspmacs/startpage-put-url-links)
        (insert "\n\n")
        (pspmacs/startpage-put-recentf)
        (insert "\n\n")
        (pspmacs/startpage-put-projects)
        (insert "\n")
        (switch-to-buffer startpage-buffer)
        (read-only-mode 1)
        (pspmacs/startpage-bind-jumps)))
    startpage-buffer))

(defun pspmacs/startpage-show ()
  "Switch to existing OR new startpage buffer

Returns buffer handle"
  (interactive)
  (let ((startpage-buffer (get-buffer pspmacs/startpage-buffer-name)))
    (if startpage-buffer
        (switch-to-buffer startpage-buffer)
      (setq startpage-buffer (pspmacs/startpage-refresh)))
    startpage-buffer))

(defun pspmacs/startpage-display ()
  "Switch to existing OR new startpage buffer

And then, forcefully run `pspmacs/startpage-refres'"
  (interactive)
  (pspmacs/startpage-show)
  (pspmacs/startpage-refresh))

(customize-set-variable 'inhibit-startpage-screen t)
(customize-set-variable 'initial-buffer-choice #'pspmacs/startpage-show)
(if (daemonp)
    (add-hook 'server-after-make-frame-hook #'pspmacs/startpage-display))

(provide 'pspmacs/startpage)
;;; startpage.el ends there

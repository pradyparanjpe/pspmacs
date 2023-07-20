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
  (expand-file-name "data/banners/1.txt" user-emacs-directory)
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
  '((:foreground "#ffffff"))
  "Links face properties"
  :type 'plist
  :group 'startpage)

(defcustom pspmacs/startpage-url-links-face-props
  '((:foreground "#9fdfff"))
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
  '((:foreground "#af9fa7"))
  "Quicklink block Link item faces"
  :type 'plist
  :group 'startpage)

(defcustom pspmacs/startpage-load-time-face-props
  '((:foreground "#bfdfff")
    (:background "#002040")
    (italic))
  "Load-time information face"
  :type 'plist
  :group 'startpage)

(defun pspmacs/startpage--shorten-path (filepath)
  "Shorten FILEPATH replacing home-directory by ~"
  (replace-regexp-in-string (getenv "HOME") "~" filepath))

(defun pspmacs/startpage--ascii-banner ()
  "Put ASCII Banner for non-graphic frames"
  (let* ((banner (split-string (f-read pspmacs/startpage-banner-ascii) "\n"))
         (banner-width (length (nth 0 banner)))
         (pad-string (pspmacs/startpage--center-pad-string banner-width))
         (render-banner (mapcan
                         (lambda (line)
                           `(,pad-string
                             ,line))
                         banner)))
    (when (> (frame-width) banner-width)

      (eval `(fancy-splash-insert
            :face pspmacs/startpage-banner-face-props
            ,@render-banner)))))

(defun pspmacs/startpage--graphic-banner ()
  "Put Image Banner for graphic frames"
  (let* ((width (round (* pspmacs/startpage-banner-scale-width
                          (frame-width))))
         (banner (create-image
                  pspmacs/startpage-banner-image
                  nil nil :width width))
         (pad-string (pspmacs/startpage--center-pad-string
                      (car (image-size banner)))))
    (fancy-splash-insert pad-string)
    (insert-image banner)))

(defun pspmacs/startpage--evil-bind-jumps (recent project)
  "Bind following keys (evil):
r: RECENT point
p: PROJECT point
R: `pspmacs/startpage-refresh'"
  (keymap-set evil-normal-state-local-map
              (kbd "r") (lambda () (interactive) (goto-char recent)))
  (keymap-set evil-normal-state-local-map
              (kbd "p") (lambda () (interactive) (goto-char project)))
  (keymap-set evil-normal-state-local-map
              (kbd "R") 'pspmacs/startpage-refresh))

(defun pspmacs/startpage--native-bind-jumps (recent project)
  "Bind following keys (native):
r: RECENT point
p: PROJECT point
R: `pspmacs/startpage-refresh'"
  (use-local-map (copy-keymap text-mode-map))
  (local-set-key (kbd "r") (lambda () (interactive) (goto-char recent)))
  (local-set-key (kbd "p") (lambda () (interactive) (goto-char project)))
  (local-set-key (kbd "R") 'pspmacs/startpage-refresh))

(defun pspmacs/startpage--center-pad-string (display-width)
  "Left padding to center text if DISPLAY-WIDTH size"
  (concat "\n"
          (make-string
           (round (/ (max 0 (- (frame-width) display-width)) 2))
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
             `(,pad-string
               (buttonize ,(pspmacs/startpage--shorten-path fname)
                          (lambda (_button) (find-file ,fname)))))
           items)))
    (fancy-splash-insert
     :face pspmacs/startpage-block-title-face-props
     (concat (string-trim-right pad-string "  $") block-title))
    (setq block-point (point))
    (eval `(fancy-splash-insert
            :face pspmacs/startpage-block-link-face-props
            ,@recent-links))
    block-point))

(defun pspmacs/startpage-put-recentf ()
  "Place a block of recentf files

customize number `pspmacs/startpage-recentf-num'"
  (pspmacs/startpage--put-block
   recentf-list
   pspmacs/startpage-recentf-num
   "(r) Recent Files"))

(defun pspmacs/startpage-put-projects ()
  "Place a block of known projects

customize number `pspmacs/startpage-projects-num'"
  (pspmacs/startpage--put-block
   (project-known-project-roots)
   pspmacs/startpage-projects-num
   "(p) Projects"))

(defun pspmacs/startpage-put-banner ()
  "Place center-aligned banner in current buffer.

If `display-graphic-p', use `pspmacs/startpage-banner-image'
else, use `pspmacs/startpage-banner-ascii'"
  (if (display-graphic-p)
      (pspmacs/startpage--graphic-banner)
    (pspmacs/startpage--ascii-banner)))

  (defun pspmacs/startpage-bind-jumps (recent project)
    "Bind jumps to locations RECENT and PROJECT in buffer."
    (if evil-state
        (pspmacs/startpage--evil-bind-jumps recent project)
      (pspmacs/startpage--native-bind-jumps recent project)))

  (defun pspmacs/startpage-put-load-time ()
    "Load time information"
    (let* ((load-string
            (format
             (emacs-init-time
              "Loaded %%d packages in %3.2f seconds")
             (length package-selected-packages)))
           (pad-string (pspmacs/startpage--center-pad-string
                        (length load-string))))
      (fancy-splash-insert
       "\n"
       pad-string
       :face pspmacs/startpage-load-time-face-props
       load-string)))

(defun pspmacs/startpage-put-url-links ()
  "Place pspmacs links"
  (let* ((num-letters (apply '+ (mapcar (lambda (x)
                                          (+ (length (car x)) 2))
                                        pspmacs/startpage-url-links)))
         (spacer (make-string (/ (- (frame-width) num-letters)
                                 (1+ (length pspmacs/startpage-url-links)))
                              ? ))
         (links-text (mapcar (lambda (item)
                               (concat
                                spacer
                                "● "
                                (buttonize (car item)
                                           (lambda (_button)
                                             (browse-url (cdr item))))))
                             pspmacs/startpage-url-links)))
    (eval `(fancy-splash-insert
            :face pspmacs/startpage-url-links-face-props
            ,@links-text))))

(defun pspmacs/startpage-refresh ()
  "Refresh start-page"
  (interactive)
  (let ((startpage-buffer (get-buffer-create "*StartPage*"))
        (recent-point 0)
        (project-point 0))
    (with-current-buffer startpage-buffer
      (read-only-mode -1)
      (erase-buffer)
      (save-excursion
        (pspmacs/startpage-put-banner)
        (pspmacs/startpage-put-load-time)
        (fancy-splash-insert "\n\n")
        (pspmacs/startpage-put-url-links)
        (fancy-splash-insert "\n\n")
        (setq recent-point (pspmacs/startpage-put-recentf))
        (fancy-splash-insert "\n\n")
        (setq project-point (pspmacs/startpage-put-projects))
        (fancy-splash-insert "\n")
        (switch-to-buffer startpage-buffer)
        (read-only-mode 1)
        (pspmacs/startpage-bind-jumps recent-point project-point)))))

(defun pspmacs/startpage-show ()
  "Switch to existing OR new startpage buffer"
  (interactive)
  (let ((startpage-buffer (get-buffer "*StartPage*")))
    (if startpage-buffer
        (switch-to-buffer startpage-buffer)
      (pspmacs/startpage-refresh))))

(defun pspmacs/startpage-display ()
  "Switch to existing OR new startpage buffer

And then, forcefully run `pspmacs/startpage-refres'"
  (interactive)
  (pspmacs/startpage-show)
  (pspmacs/startpage-refresh))

(customize-set-variable 'inhibit-startpage-screen t)
(if (daemonp)
    (add-hook 'server-after-make-frame-hook #'pspmacs/startpage-display)
  (add-hook 'emacs-startup-hook #'pspmacs/startpage-display))

(provide 'pspmacs/startpage)
;;; startpage.el ends there

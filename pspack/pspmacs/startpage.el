;;; startpage.el --- startpage modeline -*- lexical-binding: t; -*-
;;; Commentary:
;;
;; STARTPAGE file-system awareness for pspmacs
;;
;;; Code:
(defgroup startpage nil
  "STARTPAGE for pspmacs."
  :group 'pspmacs
  :prefix "pspmacs/startpage")

(require 'f)
(require 'recentf)
(require 'pspmacs/common)
(recentf-mode 1)

(defcustom pspmacs/startpage-buffer-name "*StartPage*"
  "Name of startpage buffer."
  :type 'string
  :group 'startpage)

(defcustom pspmacs/startpage-banner-image
  (expand-file-name "data/banners/Tux.svg" user-emacs-directory)
  "Banner Image."
  :type '(file :must-match t)
  :group 'startpage)

(defcustom pspmacs/startpage-banner-scale-width 1.5
  "Width of `pspmacs/startpage-banner-image' in pixels.

Set as this factor times frame width in columns."
  :type '(float :tag "X # columns (px)")
  :group 'startpage)

(defcustom pspmacs/startpage-banner-ascii
  (expand-file-name "data/banners/2.txt" user-emacs-directory)
  "Banner image."
  :type '(file :must-match t)
  :group 'startpage)

(defcustom pspmacs/startpage-dont-see
  (mapcar (lambda (x) (expand-file-name (file-name-as-directory x)))
          `(,package-user-dir
            ,(or (getenv "XDG_CACHE_HOME")
                 (expand-file-name ".cache" (getenv "HOME")))
            ,(or (getenv "XDG_STATE_HOME")
                 (expand-file-name ".local/state" (getenv "HOME")))))
  "Don't even look at anything placed inside this location.

This is recursive."
  :type '(repeat directory)
  :group 'startpage)

(defcustom pspmacs/startpage-emacs-roots
  (mapcar #'expand-file-name
          (remq nil `(,(and (boundp 'local-emacs-dir) local-emacs-dir)
                      ,(and (boundp 'pvt-emacs-dir) pvt-emacs-dir)
                      ,user-emacs-directory
                      ;; ,(file-name-as-directory (getenv "HOME"))
                      ,(file-name-as-directory
                        (expand-file-name ".local/share" (getenv "HOME")))
                      ,(file-name-as-directory
                        (expand-file-name
                         "emacs/src"
                         (file-name-as-directory
                          (or (getenv "XDG_DATA_HOME")
                              (expand-file-name
                               ".local/share" (getenv "HOME")))))))))
  "List of file system locations, which are known roots for system files.

`recentf-list' files located in any of these locations are hidden (recursive).
Known project locations which are known as roots through this list are hidden.
If any entry is a file-location, *ONLY THAT* file is hidden.
Projects located \=inside\= known roots are *NOT* hidden.

Rather than customizing this variable in an init file, use helper function
`pspmacs/startpage-add-roots'

Confirm that directories have a trailing \=/\="
  :type '(repeat (choice file directory))
  :group 'startpage)

(defcustom pspmacs/startpage-ignore-file-regex
  '("\\.gpg$" "\\.elc$" "\\(.*\\)\\.#" "~$" "\\(.*\\)~" "\\.tmp$")
  "Ignore files satisfying regular these regular expressions.

Remember: absolute file names are matched; regexp matching with beginning of
file name must be of the form '\\\\(.*/\\\\)*exp' and not \=^exp\="
  :type '(repeat string)
  :group 'startpage)

(defcustom pspmacs/startpage-block-cap 0.8
  "Fraction of `window-width' as limit on length of file-names."
  :type 'number
  :group 'startpage)

(defcustom pspmacs/startpage-recentf-num 5
  "Number of recent files to link."
  :type 'integer
  :group 'startpage)

(defcustom pspmacs/startpage-projects-num 3
  "Number of projects to link."
  :type 'integer
  :group 'startpage)

(defcustom pspmacs/startpage-url-links
  '(("GNU/Emacs"
     . "https://www.gnu.org/software/emacs/manual/html_node/emacs/index.html")
    ("Emacs-lisp"
     . "https://www.gnu.org/software/emacs/manual/html_node/elisp/index.html")
    ("Documentation" . "https://pradyparanjpe.gitlab.io/pspmacs/index.html")
    ("Repository" . "https://gitlab.com/pradyparanjpe/pspmacs"))
  "Details of url links to display."
  :type '(repeat (cons (string :tag "display text")
                       (string :tag "url")))
  :group 'startpage)

(defface pspmacs/startpage-banner '((default (:foreground "#9f8f4f")))
  "Links face properties."
  :group 'startpage)

(defface pspmacs/startpage-url-links
  '((default (:foreground "#6fafcf" :underline nil)))
  "URL links face properties."
  :group 'startpage)

(defface pspmacs/startpage-block-title
  '((default (:foreground "#ff007f" :bold t)))
  "Quicklink block Title Face."
  :group 'startpage)

(defface pspmacs/startpage-block-link
  '((default (:foreground "#af9fa7" :underline nil)))
  "Quicklink block Link item faces."
  :group 'startpage)

(defface pspmacs/startpage-load-time
  '((default (:inherit italic :foreground "#bfdfff" :background "#002040")))
  "Load-time information face."
  :group 'startpage)

(defvar pspmacs/startpage-recent-files-point (point-min)
  "Point to recent files.")

(defvar pspmacs/startpage-projects-point (point-min)
  "Point to projects.")

(defun pspmacs/startpage--shorten-path (filepath)
  "Shorten FILEPATH replacing home-directory with \\='~\\='."
  (pspmacs/shorten-it
   filepath (round (* pspmacs/startpage-block-cap (window-width)))))

(defun pspmacs/startpage--ascii-banner ()
    "Put ASCII Banner for non-graphic frames."
    (let* ((banner (split-string
                    (f-read pspmacs/startpage-banner-ascii) "\n"))
           (banner-width (length (nth 0 banner)))
           (pad-string (pspmacs/startpage--center-pad-string banner-width))
           (render-banner
            (mapcan
             (lambda (line)
               (progn
                 (add-face-text-property
                  0 (length line) 'pspmacs/startpage-banner t line)
                 `(,pad-string ,line)))
             banner)))
      (when (> (window-width) banner-width)
        (eval `(insert ,@render-banner)))))

(defun pspmacs/startpage--graphic-banner ()
  "Put Image Banner for graphic frames."
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
  "Bind following EVIL keys.

<TAB> : next button
r     : RECENT point
p     : PROJECT point
`revert-buffer-function' set to `pspmacs/startpage-refresh'"
  (keymap-set evil-normal-state-local-map
              "TAB" (lambda () (interactive)
                      (forward-button 1)))
  (keymap-set evil-normal-state-local-map
              (kbd "r") (lambda () (interactive)
                          (goto-char pspmacs/startpage-recent-files-point)))
  (keymap-set evil-normal-state-local-map
              (kbd "p") (lambda () (interactive)
                          (goto-char pspmacs/startpage-projects-point)))
  (setq-local revert-buffer-function #'pspmacs/startpage-refresh))

(defun pspmacs/startpage--native-bind-jumps ()
    "Bind following keys natively.

r: RECENT point
p: PROJECT point
`revert-buffer-function' set to `pspmacs/startpage-refresh'"
    (use-local-map (copy-keymap text-mode-map))
    (local-set-key (kbd "<tab>")
                   (lambda () (interactive)
                     (forward-button 1)))
    (local-set-key (kbd "r")
                   (lambda () (interactive)
                     (goto-char pspmacs/startpage-recent-files-point)))
    (local-set-key (kbd "p")
                   (lambda () (interactive)
                     (goto-char pspmacs/startpage-projects-point)))
    (setq revert-buffer-function #'pspmacs/startpage-refresh))

(defun pspmacs/startpage--center-pad-string (display-width)
  "Left padding to center text if DISPLAY-WIDTH size."
  (concat "\n" (make-string (round (/ (max 0 (- (window-width) display-width))
                                      2))
                            ? )))

(defun pspmacs/startpage--put-links (fname-list &optional pad-string)
  "Put link to FNAME-LIST padded with PAD-STRING."
  (dolist (fname fname-list nil)
    (let ((button
           (buttonize
            (pspmacs/startpage--shorten-path fname)
            (lambda (_button) (find-file fname)))))
      (add-face-text-property
       0 (length button)
       'pspmacs/startpage-block-link
       nil button)
      (insert (or pad-string "") button))))

(defun pspmacs/startpage--put-block (block-list &optional num block-title)
  "Place center-aligned block of links.

Links to files in BLOCK-LIST are enlisted in the block.
If NUM is non-zero, only NUM elements from block list are inserted.
If BLOCK-TITLE is non-nil, it is placed as a heading to the block.
Returns point to BLOCK-TITLE"
  (let* ((block-list (or block-list '("<<< EMPTY >>>")))
         (num (if num (min (length block-list) num) (length block-list)))
         (items (cl-subseq block-list 0 num))
         (max-len (min (round (* (window-width) pspmacs/startpage-block-cap))
                       (apply #'max (mapcar (lambda (fpath) (length fpath))
                                            items))))
         (pad-string (pspmacs/startpage--center-pad-string max-len))
         (block-point nil))
    (add-face-text-property
     0 (length block-title)
     'pspmacs/startpage-block-title t block-title)
    (insert (string-trim-right pad-string "  $") block-title)

    ;; Remember this point
    (setq block-point (point))

    (pspmacs/startpage--put-links items pad-string)
    block-point))

(defun pspmacs/startpage--known-projects ()
  "Get Projects list from suitable project-manager.

Supported project-managers: project.el (builtin), projectile."
  (seq-filter
   (lambda (proj)
     (let ((proj-path (expand-file-name (file-name-as-directory proj))))
       (not (or (member proj-path pspmacs/startpage-emacs-roots)
                (seq-filter (lambda (root) (eq 0 (cl-search root proj-path)))
                            pspmacs/startpage-dont-see)))))
   (if (featurep 'projectile) (projectile-load-known-projects)
     (project-known-project-roots))))

(defun pspmacs/recentf--list ()
  "Filtered recentf list."
  (seq-filter
   (lambda (filename)
     (let ((fname (expand-file-name filename)))
       (not
        (or
         ;; Verbatim file name
         (member fname pspmacs/startpage-emacs-roots)
         ;; Regular expression match
         (seq-filter (lambda (regex) (string-match regex fname))
                     pspmacs/startpage-ignore-file-regex)
         ;; don't see
         (seq-filter (lambda (root) (eq 0 (cl-search root fname)))
                     pspmacs/startpage-dont-see)
         ;; file inside project (anywhere deep)
         (seq-filter (lambda (root) (eq 0 (cl-search root fname)))
                     pspmacs/startpage-emacs-roots)))))
   recentf-list))

(defun pspmacs/startpage-add-roots (&rest roots)
  "Add entry to `pspmacs/startpage-emacs-roots'.

Use this in configuration file \\='init.el\\='.
Add to list all ROOTS without checking if that location exists.

Directories *must* and file paths *can not* have a trailing \\='/\\='."
  (dolist (root roots nil) (add-to-list 'pspmacs/startpage-emacs-roots root)))

(defun pspmacs/startpage-put-recentf ()
  "Place a block of recentf files.

Customize number `pspmacs/startpage-recentf-num'."
  (setq pspmacs/startpage-recent-files-point
        (pspmacs/startpage--put-block (pspmacs/recentf--list)
                                      pspmacs/startpage-recentf-num
                                      "(r) Recent Files")))

(defun pspmacs/startpage-put-projects ()
  "Place a block of known projects.

Customize number `pspmacs/startpage-projects-num'."
  (setq pspmacs/startpage-projects-point
        (pspmacs/startpage--put-block (pspmacs/startpage--known-projects)
                                      pspmacs/startpage-projects-num
                                      "(p) Projects")))

(defun pspmacs/startpage-put-banner ()
  "Place center-aligned banner in current buffer.

If `display-graphic-p', use `pspmacs/startpage-banner-image'.
else, use `pspmacs/startpage-banner-ascii'."
  (if (display-graphic-p) (pspmacs/startpage--graphic-banner)
    (pspmacs/startpage--ascii-banner)))

(defun pspmacs/startpage-bind-jumps ()
  "Bind jumps to locations RECENT and PROJECT in buffer."
  (if (featurep 'evil) (pspmacs/startpage--evil-bind-jumps)
    (pspmacs/startpage--native-bind-jumps)))

(defun pspmacs/startpage-put-load-time ()
  "Load time information."
  (let* ((load-string
          (format
           (emacs-init-time
            "Loaded %%d packages in %3.2f seconds")
           (length package-activated-list)))
         (pad-string (pspmacs/startpage--center-pad-string
                      (length load-string))))
    (add-face-text-property
     0 (length load-string)
     'pspmacs/startpage-load-time t load-string)
    (insert "\n" pad-string load-string)))

(defun pspmacs/startpage-put-url-links ()
  "Place pspmacs links."
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
                                  'pspmacs/startpage-url-links
                                  nil button)
                                 (concat
                                  spacer
                                  "● "
                                  button)))
                             pspmacs/startpage-url-links)))
    (eval `(insert ,@links-text))))

(defun pspmacs/startpage-refresh (&optional _IGNORE-AUTO NOCONFIRM)
  "Refresh start-page.

To be in line with arguments passed by `revert-buffer-function',
_IGNORE-AUTO is outright ignored.
When NOCONFIRM is non-nil, do not confirm to revert.

Returns buffer handle."
  (interactive)
  (when (or NOCONFIRM (y-or-n-p "Refresh?"))
    (let ((startpage-buffer (get-buffer-create pspmacs/startpage-buffer-name)))
      (with-current-buffer startpage-buffer
        (read-only-mode -1)
        (erase-buffer)
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
        (pspmacs/startpage-bind-jumps)
        (goto-char (point-min))
        (forward-button 1)
        (special-mode)
        (when (featurep 'whitespace) (whitespace-mode -1))
        (when (featurep 'linum) (linum-mode -1))
        (when (featurep 'display-line-numbers) (display-line-numbers-mode -1))
        (when (featurep 'page-break-lines) (page-break-lines-mode 1)))
      startpage-buffer)))

;;;###autoload
(defun pspmacs/startpage-show ()
  "Switch to existing OR new startpage buffer.

Returns buffer handle."
  (interactive)
  (let ((startpage-buffer (get-buffer pspmacs/startpage-buffer-name)))
    (if startpage-buffer (switch-to-buffer startpage-buffer)
      (setq startpage-buffer (pspmacs/startpage-refresh nil t)))
    startpage-buffer))

;;;###autoload
(defun pspmacs/startpage-display ()
  "Switch to existing OR new startpage buffer.

And then, forcefully run `pspmacs/startpage-refresh'"
  (interactive)
  (pspmacs/startpage-show)
  (pspmacs/startpage-refresh nil t))

;;;###autoload
(defun pspmacs/startpage-set-up ()
  "Set up pspmacs startpage.

Call to initialize i.e. after `use-package'"
  (interactive)
  (customize-set-variable 'initial-buffer-choice #'pspmacs/startpage-show)
  (when (daemonp)
    (add-hook 'server-after-make-frame-hook #'pspmacs/startpage-display)))

(provide 'pspmacs/startpage)
;;; startpage.el ends here

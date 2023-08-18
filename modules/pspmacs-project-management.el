;;; project-management.el --- filesystem project-managementr -*- lexical-binding: t; -*-

;; Copyright © 2023  Pradyumna Swanand Paranjape

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

(use-package projectile
  :custom
  ;; Do not include straight repos (emacs packages) to project list
  (projectile-ignored-project-function
   (lambda (project-root)
     (string-prefix-p
      (expand-file-name "straight/" user-emacs-directory) project-root)))
  (projectile-switch-project-action 'projectile-commander)
  (projectile-project-root-files
   '(".envrc" ".projectile" "project.clj" "deps.edn"))
  (projectile-known-projects-file
    (expand-file-name "projectile-bookmarks.tld" xdg/emacs-cache-directory))
  (projectile-commander-methods nil)
  (projectile-cache-file (expand-file-name "projectile.cache"
                                           xdg/emacs-cache-directory))

  :config
  (defadvice projectile-project-root (around ignore-remote first activate)
    (unless (file-remote-p default-directory) ad-do-it))
  (projectile-mode)

  ;; projectile commander methods
  (def-projectile-commander-method ??
    "Commander help buffer."
    (ignore-errors (kill-buffer projectile-commander-help-buffer))
    (with-current-buffer
    (get-buffer-create projectile-commander-help-buffer)
  (insert "Projectile Commander Methods:\n\n")
  (dolist (met projectile-commander-methods)
    (insert (format "%c:\t%s\n" (car met) (cadr met))))
  (goto-char (point-min))
  (help-mode)
  (display-buffer (current-buffer) t))
    (projectile-commander))
  (def-projectile-commander-method ?t
    "Open a *shell* buffer for the project."
    (projectile-run-vterm))
  (def-projectile-commander-method ?\C-? ;; backspace
    "Go back to project selection."
    (projectile-switch-project))
  (def-projectile-commander-method ?d
    "Open project root in dired."
    (projectile-dired))
  (def-projectile-commander-method ?f
    "Find file in project."
    (projectile-find-file))
  (def-projectile-commander-method ?g
    "Git status in project."
    (projectile-vc)))

(use-package dired
  :ensure nil
  :general
  (pspmacs/leader-keys
    "jd" '(dired-jump :wk "dired"))
  :custom
  (dired-guess-shell-alist-user
   '(("\\.\\(png\\|jpe?g\\|tiff\\)" "feh" "xdg-open")
     ("\\.\\(mp[34]\\|m4a\\|ogg\\|flac\\|webm\\|mkv\\)" "mpv" "xdg-open")
     ("\\.\\(pdf\\)" "zathura")
     (".*" "xdg-open")))
  (dired-clean-up-buffers-to t)
  (dired-clean-confirm-killing-deleted-buffers t)
  :hook
  (dired-mode . hl-line-mode))

(use-package treemacs-icons-dired
  :hook (dired-mode . treemacs-icons-dired-enable-once))

(use-package dired-hacks-utils)

(use-package dired-subtree
  :custom
  (dired-subtree-line-prefix "——"))

(use-package dired-rainbow
  :config
  (progn
    (dired-rainbow-define-chmod directory "#6cb2eb" "d.*")
    (dired-rainbow-define html "#eb5286"
                          ("css" "less" "sass" "scss" "htm" "html" "jhtm"
                           "mht" "eml" "mustache" "xhtml"))
    (dired-rainbow-define xml "#f2d024"
                          ("xml" "xsd" "xsl" "xslt" "wsdl" "bib" "json"
                           "msg" "pgn" "rss" "yaml" "yml" "rdata"))
    (dired-rainbow-define document "#9561e2"
                          ("docm" "doc" "docx" "odb" "odt" "pdb" "pdf" "ps"
                           "rtf" "djvu" "epub" "odp" "ppt" "pptx"))
    (dired-rainbow-define markdown "#ffed4a"
                          ("org" "etx" "info" "markdown" "md" "mkd" "nfo"
                           "pod" "rst" "tex" "textfile" "txt"))
    (dired-rainbow-define database "#6574cd"
                          ("xlsx" "xls" "csv" "accdb" "db" "mdb" "sqlite"
                           "nc"))
    (dired-rainbow-define media "#de751f"
                          ("mp3" "mp4" "MP3" "MP4" "avi" "mpeg" "mpg" "flv"
                           "ogg" "mov" "mid" "midi" "wav" "aiff" "flac"))
    (dired-rainbow-define image "#f66d9b"
                          ("tiff" "tif" "cdr" "gif" "ico" "jpeg" "jpg" "png"
                           "psd" "eps" "svg"))
    (dired-rainbow-define log "#c17d11"
                          ("log"))
    (dired-rainbow-define shell "#f6993f"
                          ("awk" "bash" "bat" "sed" "sh" "zsh" "vim"))
    (dired-rainbow-define interpreted "#38c172"
                          ("py" "ipynb" "rb" "pl" "t" "msql" "mysql" "pgsql"
                           "sql" "r" "clj" "cljs" "scala" "js"))
    (dired-rainbow-define compiled "#4dc0b5"
                          ("asm" "cl" "lisp" "el" "c" "h" "c++" "h++" "hpp"
                           "hxx" "m" "cc" "cs" "cp" "cpp" "go" "f" "for"
                           "ftn" "f90" "f95" "f03" "f08" "s" "rs" "hi" "hs"
                           "pyc" ".java"))
    (dired-rainbow-define executable "#8cc4ff" ("exe" "msi"))
    (dired-rainbow-define compressed "#51d88a"
                          ("7z" "zip" "bz2" "tgz" "txz" "gz" "xz" "z" "Z"
                           "jar" "war" "ear" "rar" "sar" "xpi" "apk"
                           "xz" "tar"))
    (dired-rainbow-define packaged "#faad63"
                          ("deb" "rpm" "apk" "jad" "jar" "cab" "pak" "pk3"
                           "vdf" "vpk" "bsp"))
    (dired-rainbow-define encrypted "#ffed4a"
                          ("gpg" "pgp" "asc" "bfe" "enc" "signature" "sig"
                           "p12" "pem"))
    (dired-rainbow-define fonts "#6cb2eb"
                          ("afm" "fon" "fnt" "pfb" "pfm" "ttf" "otf"))
    (dired-rainbow-define partition "#e3342f"
                          ("dmg" "iso" "bin" "nrg" "qcow" "toast" "vcd"
                           "vmdk" "bak"))
    (dired-rainbow-define vc "#0074d9"
                          ("git" "gitignore" "gitattributes" "gitmodules"))
    (dired-rainbow-define-chmod executable-unix "#38c172" "-.*x.*")))
 (use-package dired-collapse)

(use-package dired-hide-dotfiles
  :general
  (pspmacs/local-leader-keys
    :keymaps 'dired-mode-map
    :states 'normal
    "H" 'dired-hide-dotfiles-mode)
  :hook
  (dired-mode . dired-hide-dotfiles-mode))

(use-package dired-rsync
  :general
  (pspmacs/local-leader-keys
    :keymaps 'dired-mode-map
    :states 'normal
    "r" 'dired-rsync))

(use-package dired-git
  :hook
  (dired-mode . dired-git-mode))

(use-package dired-du
  :init
  ;; indexing audit
  (when (executable-find "duc")
    (run-with-timer
      0
      3600
      (defun index-duc ()
        (start-process "duc" nil "duc" "index" (getenv "HOME")))))
  :custom
  (dired-du-size-format t)
  :config
  (when (and (executable-find "duc")
             (not (string-match-p "Error"
                                  (shell-command-to-string "duc info"))))
    (customize-set-variable 'dired-du-used-space-program '("duc" "ls -bD")))
  :hook
  (dired-mode . dired-du-mode))

(use-package emacs
  :init
  (mkdir (expand-file-name "backups" xdg/emacs-data-directory) t)
  (mkdir (expand-file-name "auto-saves" xdg/emacs-state-directory) t)
  :custom
  (dired-listing-switches "-lah")
  (backup-directory-alist
   `((".*" . ,(expand-file-name "backups" xdg/emacs-data-directory))))
  (auto-save-file-name-transforms
   `((".*" ,(file-name-directory
             (expand-file-name "auto-saves/" xdg/emacs-state-directory))
      t)))
  (auto-save-list-file-prefix (expand-file-name
                               "auto-saves/sessions"
                               xdg/emacs-state-directory)))

(pspmacs/load-inherit)

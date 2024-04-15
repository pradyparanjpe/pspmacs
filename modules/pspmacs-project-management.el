;;; project-management.el --- filesystem project-managementr -*- lexical-binding: t; -*-

;; Copyright © 2023-2024  Pradyumna Swanand Paranjape

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

(use-package project
  :custom
  (project-switch-commands
   '((project-shell-command "Shell command")
     (project-dired "Dired")
     (project-switch-to-buffer "Buffer")
     (project-find-dir "Find directory")
     (project-eshell "Eshell")
     (project-find-file "Find file")
     (project-find-regexp "Find regexp")
     (project-kill-buffers "Kill buffers")
     (project-query-replace-regexp "Query replace")
     (project-vc-dir "VC-Dir")))
  (project-list-file (xdg/make-path "projects" 'cache))
  :general
  (pspmacs/leader-keys "p" '(:keymap project-prefix-map :wk "roj")))

(use-package dired
  :ensure nil
  :general
  (pspmacs/leader-keys
    "jd" '(dired-jump :wk "ired"))
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
    "H" '(dired-hide-dotfiles-mode :wk "ide"))
  :hook
  (dired-mode . dired-hide-dotfiles-mode))

(use-package dired-rsync
  :general
  (pspmacs/local-leader-keys
    :keymaps 'dired-mode-map
    :states 'normal
    "r" '(dired-rsync :wk "sync")))

(use-package dired-git
  :hook
  (dired-mode . dired-git-mode))

(use-package dired-du
  :init
  ;; indexing audit
  (when (executable-find "duc")
    (run-with-timer 0 3600 'pspmacs/index-duc))
  :custom
  (dired-du-size-format t)
  :config
  (when (and (executable-find "duc")
             (not (string-match-p "Error"
                                  (shell-command-to-string "duc info"))))
    (customize-set-variable 'dired-du-used-space-program '("duc" "ls -bD")))
  :hook
  (dired-mode . dired-du-mode))

(use-package breadcrumb
  :demand t
  :init (breadcrumb-mode))

(use-package emacs
  :init
  (mkdir (xdg/make-path "backups") t)
  (mkdir (xdg/make-path "auto-saves" 'state) t)
  (pspmacs/extend-list 'project-vc-extra-root-markers
                       '(
                         ;; projectile
                         ".project.el" ".projectile" "project.clj"
                         ;; GNU/gcc makefile
                         "autogen.sh" "Makefile"
                         ;; python
                         "setup.py" "setup.cfg"
                         ;; cargo
                         "Cargo.toml"
                         ".envrc"
                         ))
  (require 'recentf)
  (pspmacs/extend-list 'recentf-exclude
                       (mapcar (lambda (x) (format "%s.*\\'" x))
                               pspmacs/worktrees))
  :custom
  (recentf-max-saved-items 200)
  (dired-listing-switches "-lah")
  (backup-directory-alist `((".*" . ,(xdg/make-path "backups"))))
  (auto-save-file-name-transforms
   `((".*" ,(file-name-directory (xdg/make-path "auto-saves/" 'state)) t)))
  (auto-save-list-file-prefix (xdg/make-path "auto-saves/sessions" 'state)))

(pspmacs/load-inherit)

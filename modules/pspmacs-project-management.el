;;;; project-management.el --- filesystem project-managementr -*- lexical-binding: t; -*-

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

(use-package projectile
  :general
  ;; window bindings
  (pspmacs/leader-keys
    :states 'normal
    "p" '(:ignore t :keymap projectile-command-map :which-key "projectile")
    "p <escape>" 'keyboard-escape-quit
    "p a" '(projectile-add-known-project :wk "add known")
    "p F" '(pspmacs/projectile-find-file-all :wk "find file (all)")
    "p t" '(projectile-run-vterm :wk "term"))
  :init
  (when (file-directory-p "~/git")
    (setq projectile-project-search-path '("~/git")))
  (setq projectile-completion-system 'default)
  (setq projectile-known-projects-file
    (expand-file-name "projectile-bookmarks.tld" xdg/emacs-cache-directory))
  (setq projectile-project-root-files
    '(".envrc" ".projectile" "project.clj" "deps.edn"))
  (setq projectile-switch-project-action 'projectile-commander)

  ;; Do not include straight repos (emacs packages) to project list
  (setq
   projectile-ignored-project-function
   (lambda (project-root)
     (string-prefix-p
      (expand-file-name "straight/" user-emacs-directory) project-root)))

  :config
  (defadvice projectile-project-root (around ignore-remote first activate)
    (unless (file-remote-p default-directory) ad-do-it))
  (projectile-mode)
  ;; projectile commander methods
  (setq projectile-commander-methods nil)
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

(use-package dired-hacks-utils)
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
  :hook (dired-mode . dired-hide-dotfiles-mode)
  :general
  (pspmacs/local-leader-keys
    :keymaps 'dired-mode-map
    :states 'normal
    "H" 'dired-hide-dotfiles-mode))

(use-package dired-rsync
  :general
  (pspmacs/local-leader-keys
    :keymaps 'dired-mode-map
    :states 'normal
    "r" 'dired-rsync))

(use-package dired-git
  :hook
  (dired-mode . dired-git-mode))

(use-package dired
  :straight (:type built-in)
  :custom
  (dired-listing-switches "-lah")
  :general (pspmacs/leader-keys
             "d" '(:ignore t :wk "dired")
             "dd" '(dired :wk "open")))

(use-package treemacs
  :ensure t
  :defer t
  :init
  (pspmacs/extend-list 'recentf-exclude
                       '(".*treemacs-persist\\'"
                         ".*straight/build\\'"
                         "/usr/share/emacs/.*\\'"))
  :general
  (pspmacs/leader-keys
    "0" '(treemacs-select-window :wk "treemacs"))
  :config
  (progn
    (setq treemacs-collapse-dirs                   (if treemacs-python-executable 3 0)
          treemacs-deferred-git-apply-delay        0.5
          treemacs-directory-name-transformer      #'identity
          treemacs-display-in-side-window          t
          treemacs-eldoc-display                   'simple
          treemacs-file-event-delay                2000
          treemacs-file-extension-regex            treemacs-last-period-regex-value
          treemacs-file-follow-delay               0.2
          treemacs-file-name-transformer           #'identity
          treemacs-follow-after-init               t
          treemacs-expand-after-init               t
          treemacs-find-workspace-method           'find-for-file-or-pick-first
          treemacs-git-command-pipe                ""
          treemacs-goto-tag-strategy               'refetch-index
          treemacs-header-scroll-indicators        '(nil . "^^^^^^")
          treemacs-hide-dot-git-directory          t
          treemacs-indentation                     2
          treemacs-indentation-string              " "
          treemacs-is-never-other-window           nil
          treemacs-max-git-entries                 5000
          treemacs-missing-project-action          'ask
          treemacs-move-forward-on-expand          nil
          treemacs-no-png-images                   t
          treemacs-no-delete-other-windows         t
          treemacs-project-follow-cleanup          nil
          treemacs-persist-file                    (expand-file-name "treemacs-persist" xdg/emacs-cache-directory)
          treemacs-position                        'left
          treemacs-read-string-input               'from-child-frame
          treemacs-recenter-distance               0.1
          treemacs-recenter-after-file-follow      nil
          treemacs-recenter-after-tag-follow       nil
          treemacs-recenter-after-project-jump     'always
          treemacs-recenter-after-project-expand   'on-distance
          treemacs-litter-directories              '("/node_modules" "/.venv" "/.cask")
          treemacs-project-follow-into-home        nil
          treemacs-show-cursor                     nil
          treemacs-show-hidden-files               t
          treemacs-silent-filewatch                nil
          treemacs-silent-refresh                  nil
          treemacs-sorting                         'alphabetic-asc
          treemacs-select-when-already-in-treemacs 'move-back
          treemacs-space-between-root-nodes        t
          treemacs-tag-follow-cleanup              t
          treemacs-tag-follow-delay                1.5
          treemacs-text-scale                      nil
          treemacs-user-mode-line-format           nil
          treemacs-user-header-line-format         nil
          treemacs-wide-toggle-width               70
          treemacs-width                           35
          treemacs-width-increment                 1
          treemacs-width-is-initially-locked       t
          treemacs-workspace-switch-cleanup        nil)

    ;; The default width and height of the icons is 22 pixels. If you are
    ;; using a Hi-DPI display, uncomment this to double the icon size.
    ;;(treemacs-resize-icons 44)

    (treemacs-follow-mode)
    (treemacs-filewatch-mode)
    (treemacs-fringe-indicator-mode 'always)
    (when treemacs-python-executable
      (treemacs-git-commit-diff-mode t))

    (pcase (cons (not (null (executable-find "git")))
                 (not (null treemacs-python-executable)))
      (`(t . t)
       (treemacs-git-mode 'deferred))
      (`(t . _)
       (treemacs-git-mode 'simple)))
    (treemacs-hide-gitignored-files-mode nil)))

(use-package treemacs-evil
  :after (treemacs evil)
  :ensure t)

(use-package treemacs-projectile
  :after (treemacs projectile)
  :ensure t)

(use-package treemacs-icons-dired
  :hook (dired-mode . treemacs-icons-dired-enable-once)
  :ensure t)

(use-package treemacs-magit
  :after (treemacs magit)
  :ensure t)

(use-package treemacs-tab-bar ;;treemacs-tab-bar if you use tab-bar-mode
  :after (treemacs)
  :ensure t
  :config (treemacs-set-scope-type 'Tabs))

(use-package emacs
  :init
  (mkdir (expand-file-name "backups" xdg/emacs-data-directory) t)
  (setq backup-directory-alist
        `((".*" . ,(expand-file-name "backups" xdg/emacs-data-directory))))
  (mkdir (expand-file-name "auto-saves" xdg/emacs-state-directory) t)
  (setq auto-save-file-name-transforms
        `((".*" ,(file-name-directory
                  (expand-file-name "auto-saves/" xdg/emacs-state-directory)) t)))
  (setq auto-save-list-file-prefix (expand-file-name
                                    "auto-saves/sessions"
                                    xdg/emacs-state-directory)))

(pspmacs/load-inherit)

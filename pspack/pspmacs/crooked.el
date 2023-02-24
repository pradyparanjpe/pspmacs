;;; crooked.el --- Personal crooked-definitions file -*- lexical-binding: t; -*-
;;; Commentary:
;;
;; CROOKED file-system awareness for pspmacs
;;
;;; Code:
(defgroup crooked nil
  "crooked.el is dirty `straight.el'."
  :group 'pspmacs)

(defcustom pspmacs/crooked-dir
  (file-name-as-directory
   (expand-file-name "crooked" local-emacs-directory))
  "Location to store cloned repos as an option to `straight.el'"
  :type '(string :tag "location to store cloned git repos")
  :group 'crooked)

(defcustom pspmacs/git-fetcher-plist
  '("github" "https://www.github.com"
            "gitlab" "https://www.gitlab.com"
            "sourcehut" "https://git.sr.ht"
            "codeberg" "https://codeberg.org")
  "plist of git fetcher and base url."
  :type '(repeat (cons (string :tag "fetcher")
                       (string :tag "url")))
  :group 'crooked)

(defun pspmacs/git-clone-args (melpa-style-recipe)
  "Try to create a repo url from melpa-style recipe.

MELPA-STYLE-RECIPE is parsed to create a repo url.
Default :FETCHER is github.
:HOST is interpreted as :FETCHER following `straight.el'.
:HOST supersedes :FETCHER.
:FILES, :COMMIT, :OLD-NAMES are ignored for now.
I don't know how to handle them..."
  (let* ((repo-parts (cdr melpa-style-recipe))
         (url (plist-get repo-parts :url))
         (repo (plist-get repo-parts :repo))
         (host-kw (plist-get repo-parts :host))
         (fetcher-kw (plist-get repo-parts :fetcher))
         (fetcher
          (lax-plist-get pspmacs/git-fetcher-plist
                         (or (if (and host-kw (symbolp host-kw))
                                 (symbol-name host-kw)
                               host-kw)
                             (if (and fetcher-kw (symbolp fetcher-kw))
                                 (symbol-name fetcher-kw)
                               fetcher-kw)
                             "github")))
         ;; (commit (plist-get repo-parts :commit))
         ;; (files (plist-get repo-parts :files))
         ;; (old-names (plist-get repo-parts :old-names))
         (branch (plist-get repo-parts :branch))
         (clone-args `(,(cond
                         ((and url (or (file-readable-p url)
                                       (url-https-file-readable-p url)))
                          url)
                         ((and repo (or (file-readable-p repo)
                                        (condition-case nil
                                            (url-https-file-readable-p repo)
                                          (error nil))))
                          repo)
                         (t (format "%s/%s" fetcher repo))))))
    (when branch
      (push branch clone-args)
      (push "-b" clone-args))
    clone-args))

(defun pspmacs/crooked-git-clone (melpa-style-recipe)
  "Try what `straight.el' does, but crookedly.

This is a work-around to use git-cloned builds since the user does not
want to use `straight.el'.

Interpret MELPA-STYLE-RECIPE and clone repo.
Native-compile, add to `load-path'."
  (let*
      ((clone-args (pspmacs/git-clone-args melpa-style-recipe))
       (target-name (symbol-name (car melpa-style-recipe)))
       (target-path (expand-file-name target-name pspmacs/crooked-dir))
       (process-args (append '("git" nil nil nil "clone")
                             clone-args `(,target-path))))
    (unless (file-directory-p target-path)
      (message (mapconcat 'identity (remq 'nil process-args) " "))
      (unless (eq (apply 'call-process process-args) 0)
        ;; worktree must be dirty, delete it.
        (delete-directory target-path t)
        (user-error "Can't clone with %s to %s" clone-args target-path))
      ;; Error wasn't thrown, cloning must have been successful.
      (package-generate-autoloads target-name target-path)
      ;; (ignore-errors (unless no-native-compile
      ;;                  (byte-recompile-directory target-path 0)))
      )
    (add-to-list 'load-path target-path)
    target-path))

(defun pspmacs/crooked-pull (&optional only)
  "Pull all projects in `pspmacs/crooked-dir' directory.

If ONLY is nil (default), pull all clones.
else, pull only the intended."
  (interactive)
  (dolist (child-dir
           (directory-files pspmacs/crooked-dir) nil)
    (let* ((only (cond ((not only) nil)
                       ((stringp only) `(,only))
                       ((listp only) only)
                       (t (user-error "ONLY may be a string, list or nil"))))
           (target-path (expand-file-name child-dir pspmacs/crooked-dir))
           (process-args `("git" nil nil nil
                           "-C" ,target-path "pull")))
      ;; target-path is a directory AND
      ;; ONLY is declared AND target-path is in it
      ;; OR ONLY is nil so assume pull-all
      (if (and
           (file-directory-p target-path)
           (if (or (not only)
                   (member target-path only))
               t))
          (when (file-directory-p (expand-file-name ".git" target-path))
            (message (mapconcat 'identity (remq 'nil process-args) " "))
            (unless (eq (apply 'call-process process-args) 0)
              (user-error "Can't pull %s" target-path))
            (ignore-errors (unless no-native-compile
                             (byte-recompile-directory target-path 0))))))))

(provide 'pspmacs/crooked)
;;; crooked.el ends there
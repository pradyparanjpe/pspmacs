(when (getenv "CI_PAGES_URL")
  (require 'package)
  (package-initialize)
  (add-to-list
   'package-archives '("elpa" . "https://elpa.gnu.org/packages/" ) t)
  (add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
  (package-refresh-contents)
  (package-install 'htmlize)
  (setq user-full-name nil))

;; org mode
(require 'org)
(require 'ox-publish)
(require 'ox-html)

(setq org-confirm-babel-evaluate nil)
(setq org-html-head-include-default-style nil)
(setq org-html-head "
<link rel=\"stylesheet\" href=\"https://cdn.datatables.net/1.10.22/css/jquery.dataTables.min.css\">

<link rel=\"stylesheet\" type=\"text/css\" href=\"https://fniessen.github.io/org-html-themes/src/readtheorg_theme/css/htmlize.css\"/>
<link rel=\"stylesheet\" type=\"text/css\" href=\"https://fniessen.github.io/org-html-themes/src/readtheorg_theme/css/readtheorg.css\"/>

<script src=\"https://ajax.googleapis.com/ajax/libs/jquery/2.1.3/jquery.min.js\"></script>
<script src=\"https://maxcdn.bootstrapcdn.com/bootstrap/3.3.4/js/bootstrap.min.js\"></script>
<script type=\"text/javascript\" src=\"https://fniessen.github.io/org-html-themes/src/lib/js/jquery.stickytableheaders.min.js\"></script>
<script type=\"text/javascript\" src=\"https://fniessen.github.io/org-html-themes/src/readtheorg_theme/js/readtheorg.js\"></script>

<style>pre.src{background:#343131;color:white;} </style>

<script src=\"https://cdnjs.cloudflare.com/ajax/libs/jquery/3.5.1/jquery.min.js\"></script>
<script src=\"https://cdn.datatables.net/1.10.22/js/jquery.dataTables.min.js\"></script>
<script> $(\"table\").DataTable(); </script>
")

(setq org-publish-project-alist
      '(("pspmacs"
         :base-directory "./"
         :base-extension "org"
         :publishing-directory "docs/"
         :publishing-function org-html-publish-to-html
         :recursive t
         :auto-sitemap t
         :auto-preamble t)

        ("org-static"
         :base-directory "./"
         :base-extension
         "css\\|js\\|svg\\|png\\|jpg\\|gif\\|pdf\\|mp3\\|ogg\\|swf"
         :publishing-directory "docs/"
         :publishing-function org-publish-attachment
         :recursive t)

        ("org" :components ("pspmacs" "org-static"))))

(defun pspmacs/publish-all ()
  "Publish everything"
  (mkdir "docs/" t)
  (org-publish-all t nil))

(unless (getenv "CI_PAGES_URL")
  (pspmacs/publish-all))

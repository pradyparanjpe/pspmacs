;;; profiles.el --- template for chemacs             -*- lexical-binding: t; -*-

;; Copyright © 2023  Pradyumna Swanand Paranjape

;; Author: Pradyumna Swanand Paranjape <pradyparanjpe@rediffmail.com>
;; Keywords: convenience

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Code similar to following should be placed in ~/.config/chemacs/profiles.el

;;; Code:

(("pspmacs" . ((user-emacs-directory . "~/.local/share/pspmacs")
           (env . (("PVT_EMACS_HOME" . "~/.local/share/private/pspmacs")
               ("LOCAL_EMACS_HOME" . "~/.local/share/private/pspmacs/local.d"))))))

;;; profiles.el ends here

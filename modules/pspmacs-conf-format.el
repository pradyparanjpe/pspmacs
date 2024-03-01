;;; pspmacs-conf-format.el --- filesystem conf-format -*- lexical-binding: t; -*-

;; Copyright © 2024  Pradyumna Swanand Paranjape

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

(use-package systemd)

(use-package dockerfile-mode
  :custom
  (dockerfile-build-command (cl-some #'executable-find '("podman" "docker")))
  :general
  (pspmacs/local-leader-keys
    "c" '(:ignore t :wk "compile")
    "cc" '(dockerfile-build-buffer :wk "build")'
    "cb" '(dockerfile-build-no-cache-buffer :wk "build w/o cache")))

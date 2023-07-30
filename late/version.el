;;; late/version.el --- package version -*- lexical-binding: t; -*-

(defconst pspmacs/version
  "0.0.2"
  "PSPMACS version")

(defun pspmacs/version ()
  "Version of pspmacs"
  (interactive)
  (message pspmacs/version))

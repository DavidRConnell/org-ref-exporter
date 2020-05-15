;;; org-ref-exporter.el -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2020 David R. Connell
;;
;; Author: David R. Connell <https://github/DavidRConnell>
;; Created: May 14, 2020
;; Modified: May 14, 2020
;; Version: 0.0.1
;; Keywords:
;; Homepage: https://github.com/DavidRConnell/org-ref-exporter
;; Package-Requires: ((emacs 28.0.50) (cl-lib "0.5") (org-ref) (pandoc "2.0"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;
;;
;;; Code:

(require 'org-ref)

(defvar orx-extension-alist
  '(("docx" . "docx")
    ("html" . "html")
    ("md" . "gfm")
    ("doku" . "dokuwiki")
    ("org" . "org")))

(defvar orx-label-re (label-re "\\\\label{\\(\\(.*?\\):.*?\\)}"))
(defvar orx-ref-re "\\\\\\(c|auto\\)ref{\\(\\(.*?\\):.*?\\)}")
(defvar orx-eq-re "\\(^\s*\\)\\(.*?\\)\\(\n\\\\end{.*?}\\)")
(defvar orx-graphicx-re "\\\\includegraphic{.*?}")

(defun orx-pandoc-command (basename from-ext to-ext &optional bib-file)
  "Generate pandoc shell command to run.
BASENAME should be file without extension, FROM-EXT and TO-EXT are the
file extensions to be converted to and from respectively and BIB-FILE
is an optional bib file for creating the bibliography."

  (if (not bib-file)
      (setq bib-file "")
    (setq bib-file (concat "--bibliography=" bib-file " ")))

  (format "pandoc %s.%s -f %s -t %s --filter=pandoc-citeproc %s-o %s.%s"
          basename from-ext
          (cdr (assoc from-ext orx-extension-alist))
          (cdr (assoc to-ext orx-extension-alist))
          bib-file
          basename to-ext))

(defun orx--get-bib ()
  "Get the bib file referenced in the current file.
If there is no bib file return nil."

  (if (> (length (org-ref-get-bibtex-keys)) 0)
      (expand-file-name (first (org-ref-find-bibliography)))
    nil))

(load "ox-org-ref.el")

(provide 'org-ref-exporter)
;;; org-ref-exporter.el ends here

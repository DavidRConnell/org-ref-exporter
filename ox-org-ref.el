;;; org-ref-ox.el --- View peers' comments in ediff -*- lexical-binding: t; -*-
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

(require 'cl-lib)
(require 'org-ref)

(defvar orx-filter-func-alist
  '(("docx" . nil)
    ("html" . nil)
    ("md" . nil)
    ("doku" . #'orx-doku-filter))
  "Filters to apply before running pandoc for type specific tweaks.")

(defvar orx-preferred-figure-types-alist
  '(("docx" . "pdf")
    ("html" . "svg")
    ("md" . "svg")
    ("doku" . "png")))

(defun org-ref-export (ext &optional async subtreep visible-only body-only options)
  (org-latex-export-to-latex async subtreep visible-only body-only options)
  (let* ((bib-file (orx--get-bib))
         (basename (file-name-sans-extension (buffer-file-name)))
         (tex-file (concat basename ".tex"))
         (new-file (concat basename "." ext))
         (buf (find-file-noselect tex-file)))

    (with-current-buffer buf
      (orx--convert-tikz-figures
       (cdr (assoc ext orx-preferred-figure-types-alist)))
      (orx--fix-references)
      (if bib-file
          (orx--add-reference-header))
      (save-buffer)
      (if (cdr (assoc ext orx-filter-func-alist))
          (funcall (cdr (assoc ext orx-filter-func-alist))))
      (kill-buffer buf))

    (shell-command (orx-pandoc-command basename "org" ext bib-file))
    (message "File converted successfully")))

(defun orx--convert-tikz-figures (figure-ext)
  "Convert all tikz figures in file to preferred FIGURE-EXT.
See \\[orx-preferred-figure-types-alist]."

  (let* ((tikz-re "\\\\input{\\(.*?\\).tikz}")
         (figure-re (concat "\\(\\\\resizebox{\\(.*?\\)}{!}{" tikz-re "}\\)"
                               "\\|"
                               "\\(" tikz-re "\\)"))
         (fig-width "[width=%s]")
         (include-command "\\\\includegraphics%s{%s.pdf}"))

    (goto-char (point-min))
    (while (re-search-forward figure-re nil t)
      (let ((basename (if (match-string 1)
                          (match-string 3)
                        (match-string 5)))
            (size (if (match-string 1)
                      (format fig-width
                              (replace-regexp-in-string "\\\\" "\\\\\\\\" (match-string 2)))
                    "")))

        (replace-match (format include-command
                               size
                               basename))

        (if (or (not (file-exists-p (concat basename figure-ext)))
                (< (orx--file-modification-time (concat basename figure-ext))
                   (orx--file-modification-time (concat basename ".tikz"))))

            (progn
              (orx--tikz-convert basename figure-ext)
              (message (concat basename ".tikz converted to pdf"))))))))

(defun orx--file-modification-time (file)
  "Posix time of last modification of FILE."
  (string-to-number (format-time-string "%s"
                      (file-attribute-modification-time
                       (file-attributes file)))))

(defun orx--tikz-converter (basename out-ext)
  "Convert BASENAME.tikz figure to a OUT-EXT type figure."
  (let ((textemplate (concat
                      "\\documentclass[tikz,convert={outfile=%1$s.%2$s}]{standalone}\n"
                      "\\usepackage{graphicx}\n"
                      "\\usepackage{pgfplots}\n"
                      "\\pgfplotsset{compat=1.16}\n"
                      "\\begin{document}\n"
                      "\\input{%1$s.tikz}\n"
                      "\\end{document}"))
        (command "latex --shell-escape"))
    (with-temp-buffer
      (insert (format textemplate basename out-ext))
      (write-file "temp.tex"))
    (shell-command (concat command " temp.tex"))
    (dolist (f (directory-files "." nil "temp\..*"))
      (delete-file f))))

(defun orx--fix-references ()
  (let ((counter-alist)
        (reference-alist)
        (label)
        (type))

    (goto-char (point-min))
    (while (re-search-forward orx-label-re nil t)
      (setq type (match-string 2))
      (setq label (match-string 1))
      (if (assoc type counter-alist)
          (setf (cdr (assoc type counter-alist))
                (+ 1 (cdr (assoc type counter-alist))))
        (push (cons type 1) counter-alist))
      (push (cons label (cdr (assoc type counter-alist)))
            reference-alist)
      (cond ((string= type "fig")
             (replace-match (format "Figure %s: "
                                    (cdr (assoc label
                                                reference-alist)))))
            ((string= type "tab")
             (replace-match (format "Table %s: "
                                    (cdr (assoc label
                                                reference-alist)))))
            ((string= type "eq")
             (progn
               (replace-match "")
               (re-search-forward eq-re nil t)
               (replace-match (format "%s%s \\\\qquad \\\\left(%s\\\\right)%s"
                                      (match-string 1)
                                      (replace-regexp-in-string "\\\\" "\\\\\\\\"
                                                                (match-string 2))
                                      (cdr (assoc label
                                                  reference-alist))
                                      (replace-regexp-in-string "\\\\" "\\\\\\\\"
                                                                (match-string 3))
                                      ))))))

    (goto-char (point-min))
    (while (re-search-forward orx-ref-re nil t)
      (setq type (cond
                  ((string= (match-string 3) "fig")
                   "Figure")
                  ((string= (match-string 3) "sec")
                   "Section")
                  ((string= (match-string 3) "tab")
                   "Table")
                  ((string= (match-string 3) "eq")
                   "eq")))

      (replace-match
       (format "%s %s"
               type
               (cdr (assoc (match-string 2) reference-alist)))))))

(defun orx--add-reference-header ()
  (goto-char (point-max))
  (re-search-backward "\\\\bibliography{.*?}" nil t)
  (if (match-string 0)
      ;; extra \\ needed because replace-match wants for but match-string returns only 2
      (replace-match (concat "\\\\section{References}\n\\" (match-string 0)))))

(defun orx-doku-filter ()
  (orx-remove-figure-sub-dirs))

(defun orx--remove-figure-sub-dirs ()
  (goto-char (point-min))
  (while (re-search-forward orx-graphicx-re nil t)
    (let ((fig-path (match-string 1)))
      (replace-match
       (replace-regexp-in-string fig-path
                                 (file-name-nondirectory fig-path)
                                 (orx--fix-backslashes (match-string 0)))))))

(defun orx--fix-backslashes (str)
  (replace-regexp-in-string "\\\\" "\\\\\\\\" str))

(defun org-ref-export-to-docx (&optional async subtreep visible-only body-only options)
  "Export current buffer to a word docx file via ox-latex and pandoc."
  (org-ref-export "docx" async subtreep visible-only body-only options))

(defun org-ref-export-to-html (&optional async subtreep visible-only body-only options)
  "Export current buffer to a word docx file via ox-latex and pandoc."
  (org-ref-export "html" async subtreep visible-only body-only options))

(defun org-ref-export-to-md (&optional async subtreep visible-only body-only options)
  "Export current buffer to a word docx file via ox-latex and pandoc."
  (org-ref-export "md" async subtreep visible-only body-only options))

(defun org-ref-export-to-doku (&optional async subtreep visible-only body-only options)
  "Export current buffer to a word docx file via ox-latex and pandoc."
  (org-ref-export "doku" async subtreep visible-only body-only options))

(org-export-define-derived-backend 'MSWord 'latex
  :menu-entry
  '(?r "Export Org-ref via LaTeX->Pandoc"
       ((?h "html" org-ref-export-to-html)
        (?m "markdown" org-ref-export-to-md)
        (?u "dokuwiki" org-ref-export-to-doku)
        (?x "docx" org-ref-export-to-docx))))

(provide 'ox-org-ref)

;;; ox-org-ref.el ends here

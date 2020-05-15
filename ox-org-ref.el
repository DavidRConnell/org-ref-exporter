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

(defun org-ref-export (ext &optional async subtreep visible-only body-only options)
  (org-latex-export-to-latex async subtreep visible-only body-only options)
  (let* ((bib-file (if (> (length (org-ref-get-bibtex-keys)) 0)
                       (expand-file-name (first (org-ref-find-bibliography)))
                     nil))
         (basename (file-name-sans-extension (buffer-file-name)))
         (tex-file (concat basename ".tex"))
         (new-file (concat basename "." ext))
         (buf (find-file-noselect tex-file)))

    (with-current-buffer buf
      (or-convert-tikz-figures)
      (or-fix-references)
      (if bib-file
          (or-add-reference-header))
      (save-buffer)
      (kill-buffer buf))

    (shell-command (orx-pandoc-command basename "org" ext bib-file))
    (message "File converted successfully")))

(defun or-convert-tikz-figures ()
  "Convert all tikz figures in file to pdf."

  (let* ((tikz-regex "\\\\input{\\(.*?\\).tikz}")
         (figure-regex (concat "\\(\\\\resizebox{\\(.*?\\)}{!}{" tikz-regex "}\\)"
                               "\\|"
                               "\\(" tikz-regex "\\)"))
         (fig-width "[width=%s]")
         (include-command "\\\\includegraphics%s{%s.pdf}"))

    (goto-char (point-min))
    (while (re-search-forward figure-regex nil t)
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

        (if (or (not (file-exists-p (concat basename ".pdf")))
                (< (or-file-modification-time (concat basename ".pdf"))
                   (or-file-modification-time (concat basename ".tikz"))))

            (progn
              (or-tikz2pdf basename)
              (message (concat basename ".tikz converted to pdf"))))))))

(defun or-file-modification-time (file)
  "Posix time of last modification."
  (string-to-number (format-time-string "%s"
                      (file-attribute-modification-time
                       (file-attributes file)))))

(defun or-tikz2pdf (basename)
  "Convert tikz figure FILENAME to a png file."
  (let ((textemplate (concat
                      "\\documentclass[tikz,convert={outfile=%1$s.pdf}]{standalone}\n"
                      "\\usepackage{graphicx}\n"
                      "\\usepackage{pgfplots}\n"
                      "\\pgfplotsset{compat=1.16}\n"
                      "\\begin{document}\n"
                      "\\input{%1$s.tikz}\n"
                      "\\end{document}"))
        (command "latex --shell-escape"))
    (with-temp-buffer
      (insert (format textemplate basename))
      (write-file "temp.tex"))
    (shell-command (concat command " temp.tex"))
    ;; (dolist (f (directory-files "." nil "temp\..*"))
    ;;   (delete-file f))
    ))

(defun or-fix-references ()
  (message "Fixing references")
  (let ((label-re "\\\\label{\\(\\(.*?\\):.*?\\)}")
        (ref-re "\\\\cref{\\(\\(.*?\\):.*?\\)}")
        (eq-re "\\(^\s*\\)\\(.*?\\)\\(\n\\\\end{.*?}\\)")
        (counter-alist)
        (reference-alist)
        (label)
        (type))
    (goto-char (point-min))
    (while (re-search-forward label-re nil t)
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
    (while (re-search-forward ref-re nil t)
      (setq type (cond
                  ((string= (match-string 2) "fig")
                   "Figure")
                  ((string= (match-string 2) "sec")
                   "Section")
                  ((string= (match-string 2) "tab")
                   "Table")
                  ((string= (match-string 2) "eq")
                   "eq")))

      (replace-match (format "%s %s"
                             type
                             (cdr
                              (assoc (match-string 1) reference-alist)))))))

(defun or-add-reference-header ()
  (goto-char (point-max))
  (re-search-backward "\\\\bibliography{.*?}" nil t)
  (if (match-string 0)
      ;; extra \\ needed because replace-match wants for but match-string returns only 2
      (replace-match (concat "\\\\section{References}\n\\" (match-string 0)))))

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

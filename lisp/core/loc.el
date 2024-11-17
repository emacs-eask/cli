;;; core/loc.el --- Print LOC information  -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; Command use to print loc (accept multiple)
;;
;;   $ eask loc
;;
;;
;;  Positionals:
;;
;;    [files..]     specify files to print LOC information
;;

;;; Code:

(let ((dir (file-name-directory (nth 1 (member "-scriptload" command-line-args)))))
  (load (expand-file-name "_prepare.el"
                          (locate-dominating-file dir "_prepare.el"))
        nil t))

(defvar eask-loc--lines 0)
(defvar eask-loc--chars 0)

(defun eask-loc--file (file)
  "Insert LOC information for FILE."
  (unless (file-directory-p file)
    (let ((lines) (chars))
      (with-temp-buffer
        (insert-file-contents file)
        (setq lines (line-number-at-pos (point-max))
              chars (point-max)))
      (cl-incf eask-loc--lines lines)
      (cl-incf eask-loc--chars chars)
      (insert (format "| %s | %s | %s |\n" file lines chars)))))

(eask-start
  ;; Preparation
  (eask-with-archives '("gnu" "melpa")
    (eask-package-install 'markdown-mode))

  (require 'markdown-mode)
  ;; Start LOC
  (if-let* ((files (or (eask-expand-file-specs (eask-args))
                      (eask-package-files)))
           (eask-output (get-buffer-create "*eask-output*")))
      (with-current-buffer eask-output
        (erase-buffer)
        (progn  ; Print header
          (insert (format "|---|---|---|\n"))
          (insert (format "| File | Lines | Characters |\n"))
          (insert (format "|---|---|---|\n")))
        (eask-with-progress
          "Retrieving LOC information... "
          (mapc #'eask-loc--file files)
          "done ✓")
        (progn  ; Print total
          (insert (format "|---|---|---|\n"))
          (insert (format "| Total | %s | %s |\n" eask-loc--lines eask-loc--chars)))
        (progn  ; Organize
          (goto-char (point-min))
          (markdown-table-align))
        (eask-println "")
        (eask-print (buffer-string))
        (eask-println ""))
    (eask-info "(No LOC information to print.)")))

;;; core/loc.el ends here

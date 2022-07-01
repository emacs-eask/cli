;;; checkdoc.el --- Run checkdoc  -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; Commmand use to run `checkdoc' for all files
;;
;;   $ eask lint checkdoc [files..]
;;
;;
;;  Initialization options:
;;
;;    [files..]     files you want checkdoc to run on
;;

;;; Code:

(load (expand-file-name
       "../_prepare.el"
       (file-name-directory (nth 1 (member "-scriptload" command-line-args))))
      nil t)

(defvar eask--checkdoc-errors nil "Error flag.")

(defun eask--checkdoc-print-error (text start end &optional unfixable)
  "Print error for checkdoc."
  (setq eask--checkdoc-errors t)
  (let ((msg (concat (checkdoc-buffer-label) ":"
                     (int-to-string (count-lines (point-min) (or start (point-min))))
                     ": " text)))
    (if (eask-strict-p) (error msg) (warn msg))
    ;; Return nil because we *are* generating a buffered list of errors.
    nil))

(setq checkdoc-create-error-function #'eask--checkdoc-print-error)

(defun eask--checkdoc-file (filename)
  "Run checkdoc on FILENAME."
  (let* ((filename (expand-file-name filename))
         (file (eask-root-del filename))
         (eask--checkdoc-errors))
    (eask-msg "")
    (eask-msg "`%s` with checkdoc (%s)" (ansi-green file) checkdoc-version)
    (checkdoc-file filename)
    (unless eask--checkdoc-errors (eask-msg "No issues found"))))

(eask-start
  (require 'checkdoc)
  (if-let* ((files (eask-args-or-package-el-files))
            (len (length files))
            (s (eask--sinr len "" "s"))
            (have (eask--sinr len "has" "have")))
      (progn
        (mapcar #'eask--checkdoc-file files)
        (eask-info "(Total of %s file%s %s checked)" len s have))
    (eask-info "(No files have been checked (checkdoc))")
    (if (eask-args)
        (eask--print-no-matching-files)
      (eask-help "lint/checkdoc"))))

;;; checkdoc.el ends here

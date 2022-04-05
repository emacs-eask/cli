;;; checkdoc.el --- Run checkdoc  -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; Commmand use to run `checkdoc' for all files
;;
;;   $ eask checkdoc
;;
;;
;;  Initialization options:
;;
;;    [files..]     files you want checkdoc to run on
;;

;;; Code:

(load (expand-file-name
       "_prepare.el"
       (file-name-directory (nth 1 (member "-scriptload" command-line-args))))
      nil t)

(defun eask--help-checkdoc ()
  "Print help if command failed"
  (message "")
  (message "You need to specify file(s) you want the checkdoc to run on")
  (message "")
  (message "  $ eask %s FILE-1 FILE-2" (eask-command))
  (message "")
  (message "Or edit Eask file with [files] specifier")
  (message "")
  (message " [+] (files \"FILE-1\" \"FILE-2\")"))

(defvar eask--checkdoc-error nil "Error flag.")

(defun eask--checkdoc-print-error (text start end &optional unfixable)
  "Print error for checkdoc."
  (setq eask--checkdoc-error t)
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
         (file (s-replace eask-file-root "" filename))
         (eask--checkdoc-error))
    (eask-msg "")
    (eask-msg "`%s` with checkdoc (%s)" (ansi-green file) checkdoc-version)
    (checkdoc-file filename)
    (unless eask--checkdoc-error (eask-msg "No issues found"))))

(defun eask--checkdoc-file (filename)
  "Run checkdoc on FILENAME."
  (let* ((filename (expand-file-name filename))
         (file (s-replace eask-file-root "" filename))
         (eask--checkdoc-error))
    (eask-msg "")
    (eask-msg "`%s` with checkdoc (%s)" (ansi-green file) checkdoc-version)
    (checkdoc-file filename)
    (unless eask--checkdoc-error (eask-msg "No issues found"))))

(eask-start
  (require 'checkdoc)
  (if-let* ((files (or (eask-args) (eask-package-el-files)))
            (len (length files))
            (s (eask--sinr len "" "s"))
            (have (eask--sinr len "has" "have")))
      (progn
        (mapcar #'eask--checkdoc-file files)
        (eask-info "(Total of %s file%s %s checked)" len s have))
    (eask-info "(No files have been checked (checkdoc))")
    (eask--help-checkdoc)))

;;; checkdoc.el ends here

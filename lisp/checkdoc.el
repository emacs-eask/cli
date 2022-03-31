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
;;    [names..]     files you want checkdoc to run on
;;

;;; Code:

(load (expand-file-name
       "_prepare.el"
       (file-name-directory (nth 1 (member "-scriptload" command-line-args))))
      nil t)

(defun eask--checkdoc-print-error (text start end &optional unfixable)
  "Print error for checkdoc."
  (let ((msg (concat (checkdoc-buffer-label) ":"
                     (int-to-string (count-lines (point-min) (or start (point-min))))
                     ": " text)))
    (if (eask-strict-p) (error msg) (warn msg))
    ;; Return nil because we *are* generating a buffered list of errors.
    nil))

(setq checkdoc-create-error-function #'eask--checkdoc-print-error)

(defun eask--checkdoc-file (filename)
  "Run checkdoc on FILENAME."
  (let ((filename (expand-file-name filename)))
    (message "")
    (message "`%s` with checkdoc" filename)
    (checkdoc-file filename)))

(eask-start
  (if-let ((files (or (eask-args) (eask-package-el-files))))
      (mapcar #'eask--checkdoc-file files)
    (eask-info "(No files have been checked (checkdoc)")
    (eask--checkdoc-help)))

(defun eask--checkdoc-help ()
  "Print help if command failed"
  (message "")
  (message "You need to specify file(s) you want the checkdoc to run on")
  (message "")
  (message "  $ eask %s FILE-1 FILE-2" (eask-command))
  (message "")
  (message "Or edit Eask file with [files] specifier")
  (message "")
  (message " [+] (files \"FILE-1\" \"FILE-2\")"))

;;; checkdoc.el ends here

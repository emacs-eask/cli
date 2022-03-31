;;; checkdoc.el --- Run checkdoc  -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; Commmand use to run `checkdoc' for all files
;;
;;   $ eask checkdoc
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

(eask-start
  (if-let ((files (eask-args)))
      (mapcar #'checkdoc-file files)
    (message "You need to specify the file you want the checkdoc to run on")
    (message "For example,")
    (message "  eask checkdoc FILE-1 FILE-2")))

;;; checkdoc.el ends here

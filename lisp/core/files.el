;;; files.el --- Print the list of all package files  -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; Print the list of all package files
;;
;;   $ eask files
;;

;;; Code:

(load (expand-file-name
       "../_prepare.el"
       (file-name-directory (nth 1 (member "-scriptload" command-line-args))))
      nil t)

(defun eask--print-filename (filename)
  "Print out the FILENAME."
  (message "%s" filename))

(eask-start
  (let ((files (eask-package-files)))
    (mapc #'eask--print-filename files)
    (eask-info "(Total of %s item%s listed)" (length files) (eask--sinr files "" "s"))))

;;; files.el ends here

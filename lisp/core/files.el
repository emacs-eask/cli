;;; core/files.el --- Print the list of all package files  -*- lexical-binding: t; -*-

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
  (let* ((patterns (eask-args))
         (files (if patterns
                    (eask-expand-file-specs patterns)
                  (eask-package-files))))
    (mapc #'eask--print-filename files)
    (if (zerop (length files))
        (eask-info "(No package files found)")
      (eask-msg "")
      (eask-info "(Total of %s item%s listed)" (length files) (eask--sinr files "" "s")))))

;;; core/files.el ends here

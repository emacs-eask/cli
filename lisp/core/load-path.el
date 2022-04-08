;;; load-path.el --- Print the load-path from workspace  -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; Print the load-path from workspace
;;
;;   $ eask load-path
;;

;;; Code:

(load (expand-file-name
       "../_prepare.el"
       (file-name-directory (nth 1 (member "-scriptload" command-line-args))))
      nil t)

(defun eask--print-load-path (path)
  "Print out the PATH."
  (message "%s" path))

(eask-start
  (eask-pkg-init)
  (mapc #'eask--print-load-path load-path)
  (eask-info "(Total of %s load-path)" (length load-path)))

;;; load-path.el ends here

;;; exec-path.el --- Print the PATH (exec-path) from workspace  -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; Print the PATH (exec-path) from workspace
;;
;;   $ eask path
;;

;;; Code:

(load (expand-file-name
       "../_prepare.el"
       (file-name-directory (nth 1 (member "-scriptload" command-line-args))))
      nil t)

(defun eask--print-exec-path (path)
  "Print out the PATH."
  (message "%s" path))

(eask-start
  (eask-pkg-init)
  (mapc #'eask--print-exec-path exec-path)
  (eask-info "(Total of %s exec-path)" (length exec-path)))

;;; exec-path.el ends here

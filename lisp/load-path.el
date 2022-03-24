;;; load-path.el --- Print the load-path from workspace  -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; Print the load-path from workspace
;;
;;   $ eask load-path
;;
;;
;;  Effective flag:
;;
;;    [-g, --global]
;;

;;; Code:

(load-file (expand-file-name
            "_prepare.el"
            (file-name-directory (nth 1 (member "-scriptload" command-line-args)))))

(defun eask--print-load-path (path)
  "Print out the PATH."
  (message "%s" path))

(eask-start
  (eask-pkg-init)
  (mapc #'eask--print-load-path load-path))

;;; load-path.el ends here

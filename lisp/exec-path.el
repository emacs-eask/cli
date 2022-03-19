;;; exec-path.el --- Print the exec-path from workspace  -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; Print the exec-path from workspace
;;
;;   $ eask exec-path
;;

;;; Code:

(load-file (expand-file-name
            "_prepare.el"
            (file-name-directory (nth 1 (member "-scriptload" command-line-args)))))

(defun eask--print-exec-path (path)
  "Print out the PATH."
  (message "%s" path))

(eask-start
  (eask-pkg-init)
  (eask--add-bin-exec-path)
  (mapc #'eask--print-exec-path exec-path))

;;; exec-path.el ends here

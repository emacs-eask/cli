;;; exec-path.el --- Print the PATH (exec-path) from workspace  -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; Print the PATH (exec-path) from workspace
;;
;;   $ eask path
;;
;;
;;  Action options:
;;
;;    [-g]          change default workspace to `~/.emacs.d/'
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
  (mapc #'eask--print-exec-path exec-path))

;;; exec-path.el ends here

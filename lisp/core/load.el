;;; load.el --- load files  -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; Command use to load files (accept multiple)
;;
;;   $ eask load
;;

;;; Code:

(load (expand-file-name
       "../_prepare.el"
       (file-name-directory (nth 1 (member "-scriptload" command-line-args))))
      nil t)

(eask-start
  (if-let ((files (eask-expand-file-specs (eask-args))))
      (mapc #'load-file files)
    (eask-info "(Nothing to load.)")))

;;; load.el ends here

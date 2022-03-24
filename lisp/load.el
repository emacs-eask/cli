;;; load.el --- load files  -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; Command use to load files (accept multiple)
;;
;;   $ eask load
;;

;;; Code:

(load-file (expand-file-name
            "_prepare.el"
            (file-name-directory (nth 1 (member "-scriptload" command-line-args)))))

(eask-start
  (mapc #'load-file (eask-args)))

;;; load.el ends here

;;; package-directory.el --- Print path to package directory  -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; Print path to package directory,
;;
;;   $ eask package-directory
;;

;;; Code:

(load (expand-file-name
       "../_prepare.el"
       (file-name-directory (nth 1 (member "-scriptload" command-line-args))))
      nil t)

(eask-start
  (message "%s" package-user-dir))

;;; package-directory.el ends here

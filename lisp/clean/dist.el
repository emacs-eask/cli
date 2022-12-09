;;; clean/dist.el --- Do all cleaning tasks  -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; Command that cleans .eask directory, and all elc files.
;;
;;   $ eask clean dist
;;

;;; Code:

(load (expand-file-name
       "../_prepare.el"
       (file-name-directory (nth 1 (member "-scriptload" command-line-args))))
      nil t)

(eask-start
  )

;;; clean/dist.el ends here

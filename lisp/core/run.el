;;; run.el --- Run the script  -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; Command use to run scripts,
;;
;;   $ eask run <foo>
;;
;;
;;  Initialization options:
;;
;;    <foo>     run the script named <foo>
;;

;;; Code:

(load (expand-file-name
       "../_prepare.el"
       (file-name-directory (nth 1 (member "-scriptload" command-line-args))))
      nil t)

(eask-start
  )

;;; run.el ends here

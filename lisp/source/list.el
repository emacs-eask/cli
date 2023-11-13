;;; source/list.el --- List all source information  -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; Commmand use to list all source information
;;
;;   $ eask source list
;;

;;; Code:

(let ((dir (file-name-directory (nth 1 (member "-scriptload" command-line-args)))))
  (load (expand-file-name "_prepare.el"
                          (locate-dominating-file dir "_prepare.el"))
        nil t))

(eask-start
  (eask-call "core/archives"))

;;; source/list.el ends here

;;; core/package-directory.el --- Print path to package directory  -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; Print path to package directory,
;;
;;   $ eask package-directory
;;

;;; Code:

(let ((dir (file-name-directory (nth 1 (member "-scriptload" command-line-args)))))
  (load (expand-file-name "_prepare.el"
                          (locate-dominating-file dir "_prepare.el"))
        nil t))

(eask-start
  (message "%s" package-user-dir))

;;; core/package-directory.el ends here

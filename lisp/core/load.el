;;; core/load.el --- load files  -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; Command use to load files (accept multiple)
;;
;;   $ eask load
;;
;;
;;  Positionals:
;;
;;    [files..]     specify files to load
;;

;;; Code:

(let ((dir (file-name-directory (nth 1 (member "-scriptload" command-line-args)))))
  (load (expand-file-name "_prepare.el"
                          (locate-dominating-file dir "_prepare.el"))
        nil t))

(eask-start
  (if-let* ((files (eask-expand-file-specs (eask-args))))
      (mapc #'load-file files)
    (eask-info "(Nothing to load.)")))

;;; core/load.el ends here

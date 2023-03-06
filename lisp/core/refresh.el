;;; core/refresh.el --- Download package archives  -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; Command use to download package archives
;;
;;   $ eask refresh
;;

;;; Code:

(let ((dir (file-name-directory (nth 1 (member "-scriptload" command-line-args)))))
  (load (expand-file-name "_prepare.el"
                          (locate-dominating-file dir "_prepare.el"))
        nil t))

(eask-start
  (eask-pkg-init)
  (eask-msg "")
  (eask-info "(Done)"))

;;; core/refresh.el ends here

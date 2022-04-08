;;; refresh.el --- Download package archives  -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; Command use to download package archives
;;
;;   $ eask refresh
;;

;;; Code:

(load (expand-file-name
       "../_prepare.el"
       (file-name-directory (nth 1 (member "-scriptload" command-line-args))))
      nil t)

(eask-start
  (eask-pkg-init))

;;; refresh.el ends here

;;; search.el --- Search packages  -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; Command use to search Emacs packges,
;;
;;   $ eask search [queries..]
;;
;;
;;  Initialization options:
;;
;;    [queries..]     query to search packages
;;

;;; Code:

(load (expand-file-name
       "../_prepare.el"
       (file-name-directory (nth 1 (member "-scriptload" command-line-args))))
      nil t)

(eask-start
  (eask-pkg-init)
  (if-let ((queries (eask-args)))
      ))

;;; search.el ends here

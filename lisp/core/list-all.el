;;; list-all.el --- List all available packages  -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; Command use to list out available Emacs packages from archives,
;;
;;   $ eask list-all
;;
;;
;;  Action options:
;;
;;    [--depth]  dependency level to print
;;

;;; Code:

(load (expand-file-name
       "../_prepare.el"
       (file-name-directory (nth 1 (member "-scriptload" command-line-args))))
      nil t)

(eask-load "core/list")

(eask-start
  (eask-pkg-init)
  (let ((pkg-list (reverse (mapcar #'car package-archive-contents))))
    (eask--list pkg-list package-archive-contents))
  (eask-info "(Total of %s package%s available)" (length package-archive-contents)
             (eask--sinr package-archive-contents "" "s")))

;;; list-all.el ends here

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
;;    [-g]       list all packages default to `~/.emacs.d/'
;;    [--depth]  dependency level to print
;;

;;; Code:

(load-file (expand-file-name
            "_prepare.el"
            (file-name-directory (nth 1 (member "-scriptload" command-line-args)))))

(eask-load "list")

(eask-start
  (eask-pkg-init)
  (let* ((pkg-list (reverse (mapcar #'car package-archive-contents)))
         (eask-list-package-name-width (+ (eask-seq-max-str pkg-list) 1)))
    (dolist (name pkg-list)
      (eask-print-pkg name 0 (or (eask-depth) 999) package-archive-contents)))
  (message "")
  (message " Total of %s packages available" (length package-archive-contents)))

;;; list-all.el ends here

;;; clean.el --- Clean up Eask  -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; Command use to clean up `.eask' in the working directory,
;;
;;   $ eask clean [-g]
;;
;;  Action options:
;;
;;    [-g]       install packages globally to `~/.emacs.d/'
;;

;;; Code:

(load-file (expand-file-name
            "_prepare.el"
            (file-name-directory (nth 1 (member "-scriptload" command-line-args)))))

(eask-start
  (ignore-errors (delete-directory user-emacs-directory t))
  (message "[INFO] Clean up `%s`" user-emacs-directory))

;;; clean.el ends here

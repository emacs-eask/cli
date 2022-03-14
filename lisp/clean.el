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

(load-file "./lisp/_prepare.el")

(eask-start
  (ignore-errors (delete-directory user-emacs-directory t)))

;;; clean.el ends here

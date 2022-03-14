;;; clean.el --- Clean up Eask  -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; Command use to clean/remove up Emacs packages,
;;
;;   $ eask clean [-g]
;;
;; 1. Clean up `.eask' in the working directory.
;;
;; 2. If [-g] is specify; we remove `~/.emacs.d/' instead.
;;

;;; Code:

(load-file "./lisp/_prepare.el")

(eask-start
  (ignore-errors (delete-directory user-emacs-directory t)))

;;; clean.el ends here

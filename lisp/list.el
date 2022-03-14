;;; list.el --- Install packages  -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; Command use to list out Emacs packages,
;;
;;   $ eask list [-g]
;;
;;
;;  Action options:
;;
;;    [-g]       install packages globally to `~/.emacs.d/'
;;

;;; Code:

(load-file "./lisp/_prepare.el")

(eask-start
  (package-initialize)
  (dolist (pkg (reverse package-alist))
    (let* ((desc (cadr pkg))
           (name (package-desc-name desc))
           (version (package-desc-version desc))
           (summary (package-desc-summary desc)))
      (message "[+] %s %s %s" name version summary))))

;;; list.el ends here

;;; install.el --- Install packages  -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; Command use to install Emacs packages,
;;
;;   $ eask install [name] [-g]
;;
;;
;;  Initialization options:
;;
;;    [name]     name of the package to install; else we try to install package
;;               from current directory by calling function `package-install-file'
;;
;;  Action options:
;;
;;    [-g]       install packages globally to `~/.emacs.d/'
;;

;;; Code:

(load-file "./lisp/_prepare.el")

(eask-start
  (if-let* ((name (elt argv 0)) (name (intern name)))
      ;; If package [name] are specified, we try to install it
      (package-install name)
    ;; Else we try to install package from the working directory
    (package-install-file (expand-file-name "./"))))

;;; install.el ends here

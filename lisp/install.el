;;; install.el --- Install packages  -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; Command use to install Emacs packages,
;;
;;   $ eask install [name] [-g]
;;
;; If package [name] is specify; install that specific package. Otherwise, we
;; install the package in current directory using function `package-install-file'
;; if possible.
;;
;; If [-g] is specify; we install it globally to `~/.emacs.d/'.
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

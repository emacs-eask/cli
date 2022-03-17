;;; install.el --- Install packages  -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; Command use to install Emacs packages,
;;
;;   $ eask install [name] [-g] [--dev]
;;
;;
;;  Initialization options:
;;
;;    [name]     name of the package to install; else we try to install package
;;               from current directory by calling function `package-install-file'
;;
;;  Action options:
;;
;;    [-g]       install packages to default `~/.emacs.d/'
;;    [--dev]    install development packages as well
;;

;;; Code:

(load-file (expand-file-name
            "_prepare.el"
            (file-name-directory (nth 1 (member "-scriptload" command-line-args)))))

(eask-start
  (if-let* ((name (elt argv 0)) (_ (not (eask-self-command-p name))))
      ;; If package [name] are specified, we try to install it
      (eask-package-install name)
    ;; Else we try to install package from the working directory
    (eask-pkg-init)
    (mapc #'eask-package-install eask-depends-on)
    (when (eask-dev-p) (mapc #'eask-package-install eask-depends-on-dev))
    (package-install-file (expand-file-name "./"))))

;;; install.el ends here

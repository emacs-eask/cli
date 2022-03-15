;;; info.el --- Install packages  -*- lexical-binding: t; -*-

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

(load-file (expand-file-name
            "_prepare.el"
            (file-name-directory (nth 1 (member "-scriptload" command-line-args)))))

(message "%s" (emacs-version))

;;; info.el ends here

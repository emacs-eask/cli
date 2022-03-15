;;; uninstall.el --- Uninstall packages  -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; Command use to uninstall Emacs packages,
;;
;;   $ eask uninstall <name> [-g] [-f]
;;
;;
;;  Initialization options:
;;
;;    <name>     name of the package to uninstall
;;
;;  Action options:
;;
;;    [-g]       uninstall packages to default `~/.emacs.d/'
;;    [-f]       force to uninstall packages
;;

;;; Code:

(load-file (expand-file-name
            "_prepare.el"
            (file-name-directory (nth 1 (member "-scriptload" command-line-args)))))


(defun eask-package-desc (name &optional current)
  "Build package description by PKG-NAME."
  (cadr (assq name (if current package-alist package-archive-contents))))

(eask-start
  (package-initialize)
  (package-refresh-contents)
  (if-let* ((name (elt argv 0)) (name (intern name)))
      ;; If package [name] are specified, we try to install it
      (package-delete name (eask-force-p))
    ;; Else we try to install package from the working directory
    (package-install-file (expand-file-name "./"))))

;;; uninstall.el ends here

;;; outdated.el --- Show all outdated dependencies  -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; Command use to show all outdated dependencies,
;;
;;   $ eask outdated
;;
;;
;;  Action options:
;;
;;    [-g]     change default workspace to `~/.emacs.d'
;;

;;; Code:

(load-file (expand-file-name
            "_prepare.el"
            (file-name-directory (nth 1 (member "-scriptload" command-line-args)))))

(eask-load "upgrade")
(eask-load "list")

(eask-start
  (eask-pkg-init)
  (if-let* ((upgrades (eask-package--upgrades))
            (pkg-list (reverse (mapcar #'package-desc-name upgrades)))
            ;; Remove current developing packages
            (pkg-list (remove (intern (eask-guess-package-name)) pkg-list)))
      (eask--list pkg-list package-alist 0)
    (message "")
    (message " No outdated dependencies")))

;;; outdated.el ends here

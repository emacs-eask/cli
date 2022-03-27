;;; outdated.el --- Show all outdated dependencies  -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; Command use to show all outdated dependencies,
;;
;;   $ eask outdated
;;
;;
;;  Effective flag:
;;
;;    [-g, --global]
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
            (pkg-list (mapcar #'package-desc-name upgrades)))
      (progn
        (unless (eask-global-p)
          ;; Remove current developing packages
          (setq pkg-list (remove (intern (eask-guess-package-name)) pkg-list)))
        (eask--list pkg-list package-alist 0)
        (message "(Total of %s dependencies are outdated)" (length pkg-list)))
    (message "(No outdated dependencies)")))

;;; outdated.el ends here

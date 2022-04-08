;;; outdated.el --- Show all outdated dependencies  -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; Command use to show all outdated dependencies,
;;
;;   $ eask outdated
;;

;;; Code:

(load (expand-file-name
       "../_prepare.el"
       (file-name-directory (nth 1 (member "-scriptload" command-line-args))))
      nil t)

(eask-load "core/upgrade")
(eask-load "core/list")

(eask-start
  (eask-pkg-init)
  (if-let* ((upgrades (eask-package--upgrades))
            (pkg-list (mapcar #'package-desc-name upgrades)))
      (progn
        (unless (eask-global-p)
          ;; Remove current developing packages
          (setq pkg-list (remove (intern (eask-guess-package-name)) pkg-list)))
        (eask--list pkg-list package-alist 0)
        (eask-info "(Total of %s dependenc%s %s outdated)" (length pkg-list)
                   (eask--sinr pkg-list "y" "ies")
                   (eask--sinr pkg-list "is" "are")))
    (eask-info "(No outdated dependencies)")))

;;; outdated.el ends here

;;; info.el --- Display information about the current package  -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; Display information about the current package
;;
;;   $ eask info
;;

;;; Code:

(load (expand-file-name
       "_prepare.el"
       (file-name-directory (nth 1 (member "-scriptload" command-line-args))))
      nil t)

(defun eask--help-info ()
  "Print help if command failed"
  )

(eask-start
  (if eask-package
      (progn
        (eask-msg "### %s (%s) ###" (eask-package-name) (eask-package-version))
        (eask-msg "")
        (eask-msg (eask-package-description))
        (eask-msg ""))
    (eask-info "(Eask file has no package information)")
    (eask--help-info)))

;;; info.el ends here

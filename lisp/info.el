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

(defun eask--print-info (key)
  "Print package information."
  (when-let ((info (eask-package-get key)))
    (eask-msg "  ■ %s" (ansi-yellow info))))

(eask-start
  (if eask-package
      (progn
        (eask--print-info :name)
        (eask--print-info :version)
        (eask--print-info :description))
    (eask-info "(Eask file has no package information)")
    (eask--help-info)))

;;; info.el ends here

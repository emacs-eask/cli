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
        (eask-msg "")
        (eask-msg "%s (%s)" (ansi-green (eask-package-name)) (ansi-yellow (eask-package-version)))
        (eask-msg "")
        (eask-msg (eask-package-description))
        (when eask-package-file
          (eask-msg "")
          (eask-msg "entry: %s" (eask-root-del eask-package-file)))
        (when eask-depends-on
          (eask-msg "")
          (eask-msg "dependencies:")
          (dolist (dep eask-depends-on) (eask-msg "  %s" dep)))
        (when eask-depends-on-dev
          (eask-msg "")
          (eask-msg "devDependencies:")
          (dolist (dep eask-depends-on-dev) (eask-msg "  %s" dep))))
    (eask-info "(Eask file has no package information)")
    (eask--help-info)))

;;; info.el ends here

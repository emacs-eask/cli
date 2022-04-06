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

(defun eask--print-deps (title deps)
  "Print dependencies."
  (when deps
    (eask-msg "")
    (eask-msg title)
    (dolist (dep deps)
      (eask-msg "%s %s" (car dep) (cdr dep)))))

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
        (eask--print-deps "dependencies:" eask-depends-on)
        (eask--print-deps "devDependencies:" eask-depends-on-dev))
    (eask-info "(Eask file has no package information)")
    (eask--help-info)))

;;; info.el ends here

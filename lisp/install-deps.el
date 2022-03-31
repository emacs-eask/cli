;;; install-deps.el --- Automatically install package dependencies  -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; Command use to install package dependencies
;;
;;   $ eask install-deps
;;

;;; Code:

(load (expand-file-name
       "_prepare.el"
       (file-name-directory (nth 1 (member "-scriptload" command-line-args))))
      nil t)

(defun eask--help-install-deps ()
  "Print help if command failed."
  )

(eask-start
  (eask-pkg-init))

;;; install-deps.el ends here

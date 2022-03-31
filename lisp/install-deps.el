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

(eask-start
  (let ((dep eask-depends-on) (dep-dev eask-depends-on-dev))
    (cond
     ((and (not dep) (not dep-dev))
      (eask-msg "✗ (No dependencies are specify in your Eask file)")
      (eask-msg "")
      (eask-msg "You can add dependencies by using specifier [depends-on]")
      (eask-msg "")
      (eask-msg "  [+] (depends-on \"PACKAGE-NAME\")"))
     (t (eask-pkg-init)))))

;;; install-deps.el ends here

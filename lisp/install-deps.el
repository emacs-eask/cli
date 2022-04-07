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
  (eask-pkg-init)
  (cond ((not (eask-dependencies))
         (eask-msg "✗ (No dependencies are specify in your Eask file)")
         (eask-msg "")
         (eask-msg "You can add dependencies by using specifier [depends-on]")
         (eask-msg "")
         (eask-msg "  [+] (depends-on \"PKG-SPEC\")"))
        ((and (eask-dev-p) (not eask-depends-on-dev))
         (eask-msg "✗ (No development dependencies are specify in your Eask file)")
         (eask-msg "")
         (eask-msg "You can add development dependencies by wrapping [depends-on] with [development] specifier")
         (eask-msg "")
         (eask-msg "  [+] (development")
         (eask-msg "  [+]  (depends-on \"PKG-SPEC\"))"))
        (t (eask-install-dependencies))))

;;; install-deps.el ends here

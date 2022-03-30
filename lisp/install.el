;;; install.el --- Install packages  -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; Command use to install Emacs packages,
;;
;;   $ eask install [names..]
;;
;;
;;  Initialization options:
;;
;;    [names..]     name of the package to install; else we try to install
;;                  package from current directory by calling function
;;                  `package-install-file'
;;

;;; Code:

(load (expand-file-name
       "_prepare.el"
       (file-name-directory (nth 1 (member "-scriptload" command-line-args))))
      nil t)

(eask-load "package")  ; load dist path

(defun eask--install-dep ()
  "Install all dependencies."
  (mapc #'eask-package-install eask-depends-on))

(defun eask--install-dep-dev ()
  "Install all development dependencies."
  (when (eask-dev-p) (mapc #'eask-package-install eask-depends-on-dev)))

(eask-start
  (eask-pkg-init)
  (if-let ((names (eask-args)))
      ;; If package [name..] are specified, we try to install it
      (mapc #'eask-package-install names)

    ;; Else we try to install package from the working directory
    ;;
    ;; XXX Without ignore-errors guard, it will trigger error
    ;;
    ;;   Can't find library xxxxxxx.el
    ;;
    ;; But we can remove this after Emacs 28, since function `find-library-name'
    ;; has replaced the function `signal' instead of the `error'.
    (eask-ignore-errors
      (eask--install-dep)
      (eask--install-dep-dev))

    ;; Start the normal package installation procedure
    (let* ((packaged (eask-packaged-file))
           (target (or packaged
                       eask-package-file
                       (expand-file-name "./"))))
      (when packaged
        (eask-info "Found packaged artefact, use %s instead" packaged))
      (eask-info "Installing %s..." target)
      (add-to-list 'load-path (expand-file-name (eask-packaged-name) package-user-dir))
      (package-install-file target))))

;;; install.el ends here

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

(eask-start
  (eask-pkg-init)
  (if-let ((names (eask-args)))
      ;; If package [name..] are specified, we try to install it
      (eask-progress "Installing" names "... done!" #'eask-package-install)
    ;; Else we try to install package from the working directory
    (let* ((packaged (eask-packaged-file))
           (target (or packaged
                       eask-package-file
                       (expand-file-name "./"))))
      (if packaged
          (eask-info "✓ Loading packaged artefact in %s... done!" target)
        (eask-info "? Packaged artefact not found, install directly to %s..." target))
      (add-to-list 'load-path (expand-file-name (eask-packaged-name) package-user-dir))
      (package-install-file target))))

;;; install.el ends here

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

(defun eask--help-install ()
  "Print help if command failed."
  (eask-msg "")
  (eask-msg "Make sure you have specify a (package-file ..) inside your Eask file!")
  (eask-msg "")
  (eask-msg "  [+] (package-file \"PKG-MAIN.el\")"))

(defun eask--install-packages (names)
  "Install packages."
  (eask-pkg-init)
  (let* ((names (mapcar #'intern names))
         (len (length names)) (s (eask--sinr len "" "s"))
         (pkg-not-installed (cl-remove-if #'package-installed-p names))
         (installed (length pkg-not-installed)) (skipped (- len installed)))
    (eask-log "Installing specified %s package%s..." len s)
    (mapc #'eask-package-install names)
    (eask-info "(Total of %s package%s installed, %s skipped)"
               s installed skipped)))

(eask-start
  (if-let ((names (eask-args)))
      ;; If package [name..] are specified, we try to install it
      (eask--install-packages names)
    ;; Else we try to install package from the working directory
    (eask-install-dependencies)
    (eask-log "Searching for package to install in path...")
    (if-let* ((packaged (eask-packaged-file))
              (target (or packaged eask-package-file)))
        (progn
          (if packaged
              (eask-info "✓ Loading packaged artefact in %s... done!" target)
            (eask-info "? Packaged artefact not found, install directly to %s..." target))
          (add-to-list 'load-path (expand-file-name (eask-packaged-name) package-user-dir))
          (package-install-file target)
          (eask-info "✓ Done. (See %s)"
                     (file-name-directory (locate-library (eask-guess-package-name)))))
      (eask-info "✗ (No files have been intalled)")
      (eask--help-install))))

;;; install.el ends here

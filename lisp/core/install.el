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
       "../_prepare.el"
       (file-name-directory (nth 1 (member "-scriptload" command-line-args))))
      nil t)

(eask-load "core/package")  ; load dist path

(defun eask--help-install ()
  "Print help if command failed."
  (eask-msg "")
  (eask-msg "💡 Make sure you have specify a (package-file ..) inside your Eask file!")
  (eask-msg "")
  (eask-msg "    [+] (package-file \"PKG-MAIN.el\")")
  (eask-msg ""))

(defun eask--install-packages (names)
  "Install packages."
  (let* ((names (mapcar #'intern names))
         (len (length names)) (s (eask--sinr len "" "s"))
         (pkg-not-installed (cl-remove-if #'package-installed-p names))
         (installed (length pkg-not-installed)) (skipped (- len installed)))
    (eask-log "Installing %s specified package%s..." len s)
    (mapc #'eask-package-install names)
    (eask-info "(Total of %s package%s installed, %s skipped)"
               installed s skipped)))

(eask-start
  (eask-pkg-init)
  (if-let ((names (eask-args)))
      ;; If package [name..] are specified, we try to install it
      (eask--install-packages names)
    ;; Else we try to install package from the working directory
    (eask-install-dependencies)
    (let* ((name (eask-guess-package-name))
           (packaged (eask-packaged-file))
           (target (or packaged eask-package-file)))
      (eask-log "Searching for artefact to install...")
      (if packaged (eask-info "✓ Found artefact in %s" target)
        (eask-info "? Missing artefact, install directly from %s" target))
      (if target
          (progn
            (add-to-list 'load-path (expand-file-name (eask-packaged-name) package-user-dir))
            (package-install-file target)
            (eask-info "(Installed in %s)"
                       (file-name-directory (locate-library name))))
        (eask-info "✗ (No files have been intalled)")
        (eask--help-install)))))

;;; install.el ends here

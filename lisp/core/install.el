;;; core/install.el --- Install packages  -*- lexical-binding: t; -*-

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

(defun eask--install-packages (names)
  "Install packages."
  (let* ((names (mapcar #'eask-intern names))
         (len (length names)) (s (eask--sinr len "" "s"))
         (pkg-not-installed (cl-remove-if #'package-installed-p names))
         (installed (length pkg-not-installed)) (skipped (- len installed)))
    (eask-log "Installing %s specified package%s..." len s)
    (mapc #'eask-package-install names)
    (eask-msg "")
    (eask-info "(Total of %s package%s installed, %s skipped)"
               installed s skipped)))

(eask-start
  ;; XXX: You must refresh content before you install the package,
  ;; see https://github.com/ericdallo/jet.el/issues/1
  (eask-pkg-init)
  (if-let ((names (eask-args)))
      ;; If package [name..] are specified, we try to install it
      (eask--install-packages names)
    ;; Else we try to install package from the working directory
    (eask-install-dependencies)
    (let* ((name (eask-guess-package-name))
           (packaged (eask-packaged-file))
           (packaged (when (file-exists-p packaged) packaged))
           (target (or packaged eask-package-file)))
      (eask-log "Searching for artifact to install...")
      (if packaged (eask-info "✓ Found artifact in %s" target)
        (eask-info "? Missing artifact, install directly from %s" target))
      (if target
          (progn
            (add-to-list 'load-path (expand-file-name (eask-packaged-name) package-user-dir))
            (package-install-file target)
            (eask-msg "")
            (eask-info "(Installed in %s)"
                       (file-name-directory (locate-library name))))
        (eask-info "✗ (No files have been intalled)")
        (eask-help "core/install")))))

;;; core/install.el ends here

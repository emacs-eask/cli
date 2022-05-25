;;; uninstall.el --- Uninstall packages  -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; Command use to uninstall Emacs packages,
;;
;;   $ eask uninstall [names..]
;;
;;
;;  Initialization options:
;;
;;    [names..]     name of the package to uninstall; else we uninstall pacakge
;;                  from current workspace
;;

;;; Code:

(load (expand-file-name
       "../_prepare.el"
       (file-name-directory (nth 1 (member "-scriptload" command-line-args))))
      nil t)

(defun eask--uninstall-packages(names)
  "Uninstall packages."
  (let* ((names (mapcar #'intern names))
         (len (length names)) (s (eask--sinr len "" "s"))
         (pkg-installed (cl-remove-if-not #'package-installed-p names))
         (deleted (length pkg-installed)) (skipped (- len deleted)))
    (eask-log "Uninstalling %s specified package%s..." len s)
    (mapc #'eask-package-delete names)
    (eask-info "(Total of %s package%s deleted, %s skipped)"
               deleted s skipped)))

(eask-start
  (eask-defvc< 27 (eask-pkg-init))  ; XXX: remove this after we drop 26.x
  (if-let ((names (eask-args)))
      (eask--uninstall-packages names)
    (if-let* ((name (intern (eask-guess-package-name)))
              ((package-installed-p name)))
        (progn
          (eask-package-delete name)
          (eask-info "(Deleted %s)" name))
      (eask-info "✗ (No packages have been unintalled)")
      (eask-help 'uninstall))))

;;; uninstall.el ends here

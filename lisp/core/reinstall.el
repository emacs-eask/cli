;;; core/reinstall.el --- Reinstall packages  -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; Command use to reinstall Emacs packages,
;;
;;   $ eask reinstall [names..]
;;
;;
;;  Positional arguments:
;;
;;    [names..]     name of the package to reinstall
;;

;;; Code:

(let ((dir (file-name-directory (nth 1 (member "-scriptload" command-line-args)))))
  (load (expand-file-name "_prepare.el"
                          (locate-dominating-file dir "_prepare.el"))
        nil t))

(eask-load "core/package")  ; load dist path

(defun eask--reinstall-packages (names)
  "Install packages."
  (let* ((names (mapcar #'eask-intern names))
         (len (length names)) (s (eask--sinr len "" "s"))
         (pkg-not-installed (cl-remove-if #'package-installed-p names))
         (installed (length pkg-not-installed)) (skipped (- len installed)))
    (eask-log "Reinstalling %s specified package%s..." len s)
    (mapc #'eask-package-reinstall names)
    (eask-info "(Total of %s package%s reinstalled, %s skipped)"
               installed s skipped)))

(eask-start
  (eask-defvc< 27 (eask-pkg-init))  ; XXX: remove this after we drop 26.x
  (if-let ((names (eask-args)))
      ;; If package [name..] are specified, we try to install it
      (eask--reinstall-packages names)
    (if-let* ((name (intern (eask-guess-package-name)))
              ((package-installed-p name)))
        (progn
          (eask-call "core/uninstall")
          (eask-msg "")
          (eask-call "core/install")
          (eask-msg "")
          (eask-info "(Package `%s' reinstalled.)" name))
      (eask-msg "")
      (eask-info "✗ (No packages have been reintalled)")
      (eask-help "core/reinstall"))))

;;; core/reinstall.el ends here

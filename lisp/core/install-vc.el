;;; core/install-vc.el --- Install packages directly from the version control  -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; Command use to install packages directly from the version control
;;
;;   $ eask install-vc [specs..]
;;
;;
;;  Positionals:
;;
;;    [specs..]     specs to install as packages; it will install through the
;;                  function `package-vc-install'
;;

;;; Code:

(let ((dir (file-name-directory (nth 1 (member "-scriptload" command-line-args)))))
  (load (expand-file-name "_prepare.el"
                          (locate-dominating-file dir "_prepare.el"))
        nil t))

(eask-load "core/install-file")

(defun eask-install-vc--packages (specs)
  "The vc install packages with SPECS."
  (let* ((deps (mapcar (lambda (spec)
                         (list (eask-install-file--guess-name spec) spec))
                       specs))
         (names (mapcar #'car deps))
         (len (length deps))
         (s (eask--sinr len "" "s"))
         (pkg-not-installed (cl-remove-if #'package-installed-p names))
         (installed (length pkg-not-installed)) (skipped (- len installed)))
    (eask-log "Installing %s specified vc package%s..." len s)
    (eask-msg "")
    (eask--package-mapc (lambda (dep &rest _)
                          (let ((name (car dep))
                                (spec (cdr dep)))
                            (eask-package-vc-install name spec)))
                        deps)
    (eask-msg "")
    (eask-info "(Total of %s vc package%s installed, %s skipped)"
               installed s skipped)))

(eask-start
  (eask-command-check "29.1")

  (eask-pkg-init)
  (if-let* ((specs (eask-args)))
      ;; If package [specs..] are specified, we try to install it
      (eask-install-vc--packages specs)
    ;; Otherwise, report error.
    (eask-info "(No vc packages have been intalled)")
    (eask-help "core/install-vc")))

;;; core/install-vc.el ends here

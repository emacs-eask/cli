;;; install-deps.el --- Automatically install package dependencies  -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; Command use to install package dependencies
;;
;;   $ eask install-deps
;;

;;; Code:

(load (expand-file-name
       "../_prepare.el"
       (file-name-directory (nth 1 (member "-scriptload" command-line-args))))
      nil t)

(eask-start
  (if (eask-dependencies)
      (progn
        (when (and (eask-dev-p) (not eask-depends-on-dev))
          (eask-warn "No development dependencies found in your Eask file; but continue to install package dependencies"))
        (eask-install-dependencies))
    (eask-info "✗ (No dependencies found in your Eask file)")
    (eask-help 'install-deps)))

;;; install-deps.el ends here

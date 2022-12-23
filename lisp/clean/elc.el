;;; clean/elc.el --- Remove byte compiled files generated by cask build  -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; Remove byte compiled files generated by cask build
;;
;;   $ eask clean elc
;;

;;; Code:

(load (expand-file-name
       "../_prepare.el"
       (file-name-directory (nth 1 (member "-scriptload" command-line-args))))
      nil t)

(eask-start
  (if-let ((files (eask-package-elc-files)))
      (progn
        (mapc #'eask-delete-file files)
        (eask-info "✓ (Total of %s .elc file%s deleted)" (length files)
                   (eask--sinr files "" "s")))
    (eask-msg "")
    (eask-info "(No .elc file found in workspace)")))

;;; clean/elc.el ends here

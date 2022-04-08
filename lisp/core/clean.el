;;; clean.el --- Clean up .eask directory  -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; Command use to clean up `.eask' in the working directory,
;;
;;   $ eask clean [-g]
;;

;;; Code:

(load (expand-file-name
       "../_prepare.el"
       (file-name-directory (nth 1 (member "-scriptload" command-line-args))))
      nil t)

(eask-start
  (let ((target-dir
         (if (eask-global-p) user-emacs-directory
           (file-name-directory (directory-file-name user-emacs-directory)))))
    (ignore-errors (delete-directory target-dir t))
    (if eask--first-init-p
        (eask-info "(Workspace already cleaned)")
      (eask-info "✓ Done (workspace `%s` is cleaned)" target-dir))))

;;; clean.el ends here

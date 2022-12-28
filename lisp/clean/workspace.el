;;; clean/workspace.el --- Clean up .eask directory  -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; Command use to clean up `.eask' in the working directory,
;;
;;   $ eask clean workspace [-g]
;;

;;; Code:

(load (expand-file-name
       "../_prepare.el"
       (file-name-directory (nth 1 (member "-scriptload" command-line-args))))
      nil t)

(eask-start
  (let ((target-dir (if (eask-global-p) user-emacs-directory
                      (file-name-directory (directory-file-name user-emacs-directory)))))
    (unless eask--first-init-p
      (eask-msg "Deleting %s..." target-dir))
    (ignore-errors (delete-directory target-dir t))
    (if eask--first-init-p
        (progn
          (eask-info "(Workspace is already cleaned)")
          (setq eask-no-cleaning-operation-p t))
      (eask-info "✓ (Workspace is now cleaned)" target-dir))))

;;; clean/workspace.el ends here

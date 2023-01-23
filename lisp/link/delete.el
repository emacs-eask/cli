;;; link/delete.el --- Delete local linked packages  -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; Commmand use to delete local linked packages
;;
;;   $ eask link delete [names..]
;;
;;
;;  Initialization options:
;;
;;    [names..]     name of the link, accept array
;;

;;; Code:

(load (expand-file-name
       "../_prepare.el"
       (file-name-directory (nth 1 (member "-scriptload" command-line-args))))
      nil t)

(eask-load "link/list")

(defun eask--delete-symlink (path)
  "Delete symlink PATH."
  (ignore-errors (delete-file path))
  (ignore-errors (delete-directory path t)))

(defun eask--delete-link (name)
  "Delete a link by its' NAME."
  (let* ((links (eask--links))
         (source (assoc name links))
         (link (expand-file-name name package-user-dir)))
    (if (and source (file-symlink-p link))
        (progn
          (eask--delete-symlink link)
          (eask-info "✓ Unlinked package %s" link)
          t)
      (eask-info "✗ Package %s not linked" name)
      nil)))

(eask-start
  (let ((names (eask-args))
        (links (eask--links))
        (deleted 0))
    (cond ((zerop (length names))
           (eask-info "✗ No package to unlink, please specify with the package name")
           (eask-help "link/delete"))
          (t
           (dolist (name names)
             (when (eask--delete-link name)
               (cl-incf deleted)))
           (eask-msg "")
           (eask-info "(Total of %s package%s unlinked, %s skipped)"
                      deleted
                      (eask--sinr deleted "" "s")
                      (- (length names) deleted))))))

;;; link/delete.el ends here

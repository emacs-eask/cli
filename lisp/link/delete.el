;;; link/delete.el --- Delete local linked packages  -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; Commmand use to delete local linked packages
;;
;;   $ eask link delete [names..]
;;
;;
;;  Positionals:
;;
;;    [names..]     name of the link, accept array
;;

;;; Code:

(let ((dir (file-name-directory (nth 1 (member "-scriptload" command-line-args)))))
  (load (expand-file-name "_prepare.el"
                          (locate-dominating-file dir "_prepare.el"))
        nil t))

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
          (eask-info "✓ Package `%s` unlinked" name)
          t)
      (eask-info "✗ No linked package name `%s`" name)
      nil)))

(eask-start
  (let* ((names (eask-args))
         (links (eask--links))
         (link-names (mapcar #'car links))
         (deleted 0))
    (cond
     ;; Argument check.
     ((zerop (length names))
      (eask-info "✗ No package to unlink, please specify with the package name")
      (eask-help "link/delete"))
     ;; No link to delete
     ((zerop (length links))
      (eask-info "No links presented; tasks ended with no operation"))
     ;; Link not found
     ((cl-some (lambda (name)
                 (not (member name link-names)))
               names)
      (eask-info "(No operation)")
      (eask-msg "")
      (eask-info "Arguments contain invalid link name:")
      (eask-msg "")
      (dolist (name names)
        (unless (memq name link-names)
          (eask-msg "  ✗ %s" name)))
      (eask-msg "")
      (eask-call "link/list"))
     ;; Okay! Good to go!
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

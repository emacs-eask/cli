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

(defun eask--delete-link (link)
  "Delete a LINK."
  (let* ((links (eask--links))
         (link (assoc name links)))
    (if (and link (f-symlink? (cdr link)))
        (progn
          (f-delete link)
          (eask-info "Unlink package %s" (car link)))
      (eask-info "Package %s not linked" name))))

(eask-start
  (eask-with-archives "melpa"
    (eask-package-install 'f))
  (require 'f)
  (let ((names (eask-args))
        (links (eask--links)))
    (cond ((zerop names)
           (eask-info "No package to unlink, please specify with the package name")
           (eask-help "link/delete"))
          (t
           (mapc #'eask--delete-link names)))))

;;; link/delete.el ends here

;;; link/list.el --- List all project links  -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; Commmand use to list all project links
;;
;;   $ eask link list
;;

;;; Code:

(load (expand-file-name
       "../_prepare.el"
       (file-name-directory (nth 1 (member "-scriptload" command-line-args))))
      nil t)

(defun eask--links ()
  "Return a list of all links."
  (mapcar
   (lambda (file)
     (list (f-filename file) (f-canonical file)))
   (f-entries package-user-dir 'f-symlink?)))

(defun eask--print-link (link offset)
  "Print information regarding the LINK.

The argument OFFSET is used to align the result."
  (eask-msg (concat "%-" (eask-2str offset) "s  %s" (car link) (cdr link))))

(eask-start
  (eask-with-archives "melpa"
    (eask-package-install 'f))
  (require 'f)
  (if-let* ((links (eask--links))
            (offset (eask-seq-str-max (mapc #'car links))))
      (progn
        (dolist (link links)
          (eask--print-link link offset))
        (eask-info "TODO: .."))
    (eask-info "TODO: ..")))

;;; link/add.el ends here

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
     (cons (eask-f-filename file) (file-truename file)))
   (cl-remove-if-not #'file-symlink-p (directory-files package-user-dir t))))

(defun eask--print-link (link offset)
  "Print information regarding the LINK.

The argument OFFSET is used to align the result."
  (message (concat "  %-" (eask-2str offset) "s  %s") (car link) (cdr link)))

(eask-start
  (if-let* ((links (eask--links))
            (offset (eask-seq-str-max (mapcar #'car links))))
      (progn
        (eask-info "Available linked packages:")
        (eask-msg "")
        (dolist (link links) (eask--print-link link offset))
        (eask-msg "")
        (eask-info "(Total of %s linked package%s)"
                   (length links)
                   (eask--sinr links "" "s")))
    (eask-info "(No linked packages)")))

;;; link/add.el ends here

;;; eval.el --- Evaluate lisp form with a proper PATH  -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; Evaluate lisp form with a proper PATH,
;;
;;   $ eask eval [form]
;;

;;; Code:

(load (expand-file-name
       "../_prepare.el"
       (file-name-directory (nth 1 (member "-scriptload" command-line-args))))
      nil t)

(eask-start
  (eask-pkg-init)
  (let ((form (eask-argv 0)))
    (with-temp-buffer (insert form) (eval-buffer))))

;;; eval.el ends here

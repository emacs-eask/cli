;;; core/load-path.el --- Print the load-path from workspace  -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; Print the load-path from workspace
;;
;;   $ eask load-path
;;

;;; Code:

(let ((dir (file-name-directory (nth 1 (member "-scriptload" command-line-args)))))
  (load (expand-file-name "_prepare.el"
                          (locate-dominating-file dir "_prepare.el"))
        nil t))

(defun eask--print-load-path (path)
  "Print out the PATH."
  (eask-println "%s" path))

(defun eask--filter-path (path)
  "Filter the PATH out by search regex."
  (cl-some (lambda (regex)
             (string-match-p regex path))
           (eask-args)))

(eask-start
  (eask-pkg-init)
  (let* ((patterns (eask-args))
         (load-path (if patterns
                        (cl-remove-if-not #'eask--filter-path load-path)
                      load-path)))
    (eask-msg "")
    (mapc #'eask--print-load-path load-path)
    (if (zerop (length load-path))
        (eask-info "(No load-path found)")
      (eask-info "(Total of %s load-path)" (length load-path)))))

;;; core/load-path.el ends here

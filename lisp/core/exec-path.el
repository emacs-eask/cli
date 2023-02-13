;;; core/exec-path.el --- Print the PATH (exec-path) from workspace  -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; Print the PATH (exec-path) from workspace
;;
;;   $ eask path
;;

;;; Code:

(load (expand-file-name
       "../_prepare.el"
       (file-name-directory (nth 1 (member "-scriptload" command-line-args))))
      nil t)

(eask-load "core/load-path")

(defun eask--print-exec-path (path)
  "Print out the PATH."
  (message "%s" path))

(eask-start
  (eask-pkg-init)
  (let* ((patterns (eask-args))
         (exec-path (if patterns
                        (cl-remove-if-not #'eask--filter-path exec-path)
                      exec-path)))
    (eask-msg "")
    (mapc #'eask--print-exec-path exec-path)
    (if (zerop (length exec-path))
        (eask-info "(No exec-path found)")
      (eask-msg "")
      (eask-info "(Total of %s exec-path)" (length exec-path)))))

;;; core/exec-path.el ends here

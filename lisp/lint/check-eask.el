;;; check-eask.el --- Run eask checker  -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; Commmand use to run Eask checker
;;
;;   $ eask check-eask
;;

;;; Code:

(load (expand-file-name
       "../_prepare.el"
       (file-name-directory (nth 1 (member "-scriptload" command-line-args))))
      nil t)

(defvar eask--checker-log nil)

(defun eask--write-log (level msg)
  "Write the log."
  (let* ((level-string (cl-case level
                         (`error "Error")
                         (`warn  "Warning")))
         (log (format "%s:%s:%s %s: %s" (file-name-nondirectory load-file-name)
                      (line-number-at-pos) (current-column)
                      level-string
                      msg)))
    (push (ansi-color-filter-apply log) eask--checker-log)))

(add-hook 'eask-on-error-hook #'eask--write-log)
(add-hook 'eask-on-warning-hook #'eask--write-log)

(eask-start
  (mapc #'eask-msg eask--checker-log))

;;; check-eask.el ends here

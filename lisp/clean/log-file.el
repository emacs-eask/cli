;;; clean/log-file.el --- RemoveRemove all generated log files  -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; Remove all generated log files,
;;
;;   $ eask clean log-file
;;

;;; Code:

(load (expand-file-name
       "../_prepare.el"
       (file-name-directory (nth 1 (member "-scriptload" command-line-args))))
      nil t)

(defmacro eask--log-remove (file)
  "Remove log FILE."
  `(ignore-errors (delete-file (expand-file-name ,file log-dir))))

(defun eask--delete-log-file (file log-dir count)
  "Delete a log FILE."
  (when (eask-delete-file (expand-file-name file log-dir))
    (cl-incf count))
  count)

(eask-start
  (let ((log-dir (expand-file-name eask-log-path eask-file-root))
        (deleted 0)
        (log-files '("messages.log"
                     "warnings.log"
                     "backtrace.log"
                     "compile-log.log")))
    (dolist (log-file log-files)
      (setq deleted (eask--delete-log-file log-file log-dir deleted)))
    (eask-msg "")
    (if (not (zerop deleted))
        (eask-info "(Total of %s log file%s deleted, %s skipped)" deleted
                   (eask--sinr deleted "" "s")
                   (- (length log-files) deleted))
      (eask-info "(No log file found in workspace)")
      (setq eask-no-cleaning-operation-p t))))

;;; clean/log-file.el ends here

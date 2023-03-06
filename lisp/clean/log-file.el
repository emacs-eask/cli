;;; clean/log-file.el --- Remove all generated log files  -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; Remove all generated log files,
;;
;;   $ eask clean log-file
;;

;;; Code:

(let ((dir (file-name-directory (nth 1 (member "-scriptload" command-line-args)))))
  (load (expand-file-name "_prepare.el"
                          (locate-dominating-file dir "_prepare.el"))
        nil t))

(defun eask--clean-log (path)
  "Clean up .log PATH."
  (let ((log-files '("messages.log"
                     "warnings.log"
                     "backtrace.log"
                     "compile-log.log"))
        (deleted 0)
        (delete-dir))
    (dolist (log-file log-files)
      (when (eask-delete-file (expand-file-name log-file path))
        (cl-incf deleted)))
    (when (and (not (zerop deleted)) (directory-empty-p path))
      (eask-with-progress
        (format "The dist folder %s seems to be empty, delete it as well... " path)
        (ignore-errors (delete-directory path))
        "done ✓")
      (setq delete-dir t))
    (eask-msg "")
    (eask-info "(Total of %s log file%s deleted, %s skipped)" deleted
               (eask--sinr deleted "" "s")
               (- (length log-files) deleted))))

(eask-start
  (let ((log-dir (expand-file-name eask-log-path eask-file-root)))
    (if (file-directory-p log-dir)
        (eask--clean-log log-dir)
      (eask-info "(No log file found in workspace)")
      (setq eask-no-cleaning-operation-p t))))

;;; clean/log-file.el ends here

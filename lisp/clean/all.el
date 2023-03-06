;;; clean/all.el --- Do all cleaning tasks  -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; Command that cleans .eask directory, and all elc files.
;;
;;   $ eask clean all
;;

;;; Code:

(let ((dir (file-name-directory (nth 1 (member "-scriptload" command-line-args)))))
  (load (expand-file-name "_prepare.el"
                          (locate-dominating-file dir "_prepare.el"))
        nil t))

(defvar eask-no-cleaning-operation-p nil
  "Set to non-nil if there is no cleaning operation done.")

(defvar eask--clean-tasks-total 0
  "Total clean tasks.")

(defvar eask--clean-tasks-cleaned 0
  "Total cleaned tasks")

(defmacro eask--clean-section (title &rest body)
  "Print clean up TITLE and execute BODY."
  (declare (indent 1))
  `(let (eask-no-cleaning-operation-p)
     (eask-with-progress
       (format "%s... " ,title)
       (eask-with-verbosity 'debug ,@body)
       (progn
         (cl-incf eask--clean-tasks-total)
         (if eask-no-cleaning-operation-p
             "skipped ✗"
           (cl-incf eask--clean-tasks-cleaned)
           "done ✓")))))

(eask-start
  (eask--clean-section "Cleaning workspace"
    (eask-call "clean/workspace"))
  (eask--clean-section "Cleaning byte-compile files"
    (eask-call "clean/elc"))
  (eask--clean-section "Cleaning dist"
    (eask-call "clean/dist"))
  (eask--clean-section "Cleaning autoloads file"
    (eask-call "clean/autoloads"))
  (eask--clean-section "Cleaning pkg-file"
    (eask-call "clean/pkg-file"))
  (eask--clean-section "Cleaning log files"
    (eask-call "clean/log-file"))
  (eask-msg "")
  (eask-info "(Total of %s task%s cleaned, %s skipped)"
             eask--clean-tasks-cleaned
             (eask--sinr eask--clean-tasks-cleaned "" "s")
             (- eask--clean-tasks-total eask--clean-tasks-cleaned)))

;;; clean/all.el ends here

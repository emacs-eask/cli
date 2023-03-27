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

(defconst eask--clean-tasks-total 6
  "Count cleaning task.")

(defvar eask--clean-tasks-count 0
  "Count cleaning task.")

(defvar eask--clean-tasks-cleaned 0
  "Total cleaned tasks")

(defmacro eask--clean-section (title &rest body)
  "Print clean up TITLE and execute BODY."
  (declare (indent 1))
  `(let (eask-no-cleaning-operation-p)
     (cl-incf eask--clean-tasks-count)
     (eask-with-progress
       (concat (format "  - [%s/%s] " eask--clean-tasks-count eask--clean-tasks-total)
               (format "%s... " ,title))
       (eask-with-verbosity 'debug ,@body)
       (if eask-no-cleaning-operation-p
           "skipped ✗"
         (cl-incf eask--clean-tasks-cleaned)
         "done ✓"))))

(eask-start
  (eask-msg "Applying %s cleaning tasks..." eask--clean-tasks-total)
  (eask-msg "")
  (eask--clean-section (format "Cleaning %s" (ansi-green "workspace"))
    (eask-call "clean/workspace"))
  (eask--clean-section (format "Cleaning %s files" (ansi-green "byte-compile (.elc)"))
    (eask-call "clean/elc"))
  (eask--clean-section (format "Cleaning %s" (ansi-green "dist"))
    (eask-call "clean/dist"))
  (eask--clean-section (format "Cleaning %s file" (ansi-green "autoloads"))
    (eask-call "clean/autoloads"))
  (eask--clean-section  (format "Cleaning %s" (ansi-green "pkg-file"))
    (eask-call "clean/pkg-file"))
  (eask--clean-section (format "Cleaning %s files" (ansi-green "log"))
    (eask-call "clean/log-file"))
  (eask-msg "")
  (eask-info "(Total of %s task%s cleaned, %s skipped)"
             eask--clean-tasks-cleaned
             (eask--sinr eask--clean-tasks-cleaned "" "s")
             (- eask--clean-tasks-count eask--clean-tasks-cleaned)))

;;; clean/all.el ends here

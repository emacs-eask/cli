;;; clean/all.el --- Do all cleaning tasks  -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; Command that cleans .eask directory, and all elc files.
;;
;;   $ eask clean all
;;

;;; Code:

(load (expand-file-name
       "../_prepare.el"
       (file-name-directory (nth 1 (member "-scriptload" command-line-args))))
      nil t)

(defvar eask-no-cleaning-operation-p nil
  "Set to non-nil if there is no cleaning operation done.")

(defmacro eask--clean-section (title &rest body)
  "Print clean up TITLE and execute BODY."
  (declare (indent 1))
  `(let (eask-no-cleaning-operation-p)
     (eask-with-progress
       (format "%s... \n" ,title)
       (progn ,@body)
       (if eask-no-cleaning-operation-p "skipped ✗" "done ✓"))))

(eask-start
  (eask--clean-section "Cleaning workspace"
    (eask-call "clean/workspace"))
  (eask-msg "")
  (eask--clean-section "Cleaning byte-compile files"
    (eask-call "clean/elc"))
  (eask-msg "")
  (eask--clean-section "Cleaning dist"
    (eask-call "clean/dist"))
  (eask-msg "")
  (eask--clean-section "Cleaning autoloads file"
    (eask-call "clean/autoloads"))
  (eask-msg "")
  (eask--clean-section "Cleaning pkg-file"
    (eask-call "clean/pkg-file"))
  (eask-msg "")
  (eask--clean-section "Cleaning log files"
    (eask-call "clean/log-file")))

;;; clean/all.el ends here

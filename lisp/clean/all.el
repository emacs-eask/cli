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

(eask-start
  (let (eask-no-cleaning-operation-p)
    (eask-with-progress
      "Cleaning workspace... \n"
      (eask-call "clean/workspace")
      (if eask-no-cleaning-operation-p "skipped ✗" "done ✓")))
  (eask-msg "")
  (let (eask-no-cleaning-operation-p)
    (eask-with-progress
      "Cleaning byte-compile files... \n"
      (eask-call "clean/elc")
      (if eask-no-cleaning-operation-p "skipped ✗" "done ✓")))
  (eask-msg "")
  (let (eask-no-cleaning-operation-p)
    (eask-with-progress
      "Cleaning dist... \n"
      (eask-call "clean/dist")
      (if eask-no-cleaning-operation-p "skipped ✗" "done ✓")))
  (eask-msg "")
  (let (eask-no-cleaning-operation-p)
    (eask-with-progress
      "Cleaning log files... \n"
      (eask-call "clean/log-file")
      (if eask-no-cleaning-operation-p "skipped ✗" "done ✓"))))

;;; clean/all.el ends here

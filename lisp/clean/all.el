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

(eask-start
  (eask-with-progress
    "✓ Cleaning workspace...\n"
    (eask-call "clean/workspace")
    "done")
  (eask-msg "")
  (eask-with-progress
    "✓ Cleaning byte-compile files...\n"
    (eask-call "clean/elc")
    "done")
  (eask-msg "")
  (eask-with-progress
    "✓ Cleaning dist...\n"
    (eask-call "clean/dist")
    "done"))

;;; clean/all.el ends here

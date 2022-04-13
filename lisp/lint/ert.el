;;; ert.el --- Run ert tests  -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; Command to run ert tests using ert-runner,
;;
;;   $ eask ert [files..]
;;
;;
;;  Initialization options:
;;
;;    [files..]     specify files to run ert tests
;;

;;; Code:

(load (expand-file-name
       "../_prepare.el"
       (file-name-directory (nth 1 (member "-scriptload" command-line-args))))
      nil t)

(require 'ert)

(defun eask--ert-test (filename)
  "Run ert for FILENAME."
  (eask-msg "")
  (eask-msg "`%s` with ert" (ansi-green filename))
  (eask-ignore-errors (load filename t t))
  (ert-run-tests-batch)
  (ert-delete-all-tests))

(eask-start
  (eask-pkg-init)
  (if-let ((files (eask-expand-file-specs (eask-args))))
      (mapc #'eask--ert-test files)
    (eask-info "(No tests found.)")
    (eask-help 'ert)))

;;; ert.el ends here

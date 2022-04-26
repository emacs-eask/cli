;;; buttercup.el --- Run buttercup tests  -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; Command to run buttercup tests,
;;
;;   $ eask buttercup [files..]
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

(eask-start
  (eask-with-archives "melpa"
    (eask-package-install 'buttercup))
  (require 'buttercup)
  (buttercup-run-discover))

;;; buttercup.el ends here

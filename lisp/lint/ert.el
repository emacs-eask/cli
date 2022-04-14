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

(defvar ert--message-loop nil
  "Prevent inifinite recursive message function.")

(defvar ert--error nil
  "Set to t when error occured during ERT test.")

(defun eask--ert-message (func &rest args)
  "Colorized ert messages."
  (if ert--message-loop (apply func args)
    (let ((ert--message-loop t)
          (case-fold-search))
      (cond
       ((string-match-p "^[ ]+FAILED " (apply #'format args))
        (setq ert--error t)
        (eask-msg (ansi-red (apply #'format args))))
       ((string-match-p "^[ ]+passed " (apply #'format args))
        (eask-msg (ansi-green (apply #'format args))))
       (t (apply func args))))))

(advice-add 'message :around #'eask--ert-message)

(eask-start
  (if-let ((files (eask-expand-file-specs (eask-args))))
      (progn
        (eask-pkg-init)
        (eask-ignore-errors
          (mapc #'load-file files)
          (ert-run-tests-batch))
        (when ert--error
          (eask-error "ERT test failed.")))
    (eask-info "(No tests found.)")
    (eask-help 'ert)))

;;; ert.el ends here

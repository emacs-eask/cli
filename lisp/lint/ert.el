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

(setq noninteractive nil)  ; avoid redefined

;; Handle if ert doesn't exit Emacs itself.
(add-hook 'eask-after-command-hook (lambda () (kill-emacs 0)))

(eask-start
  (if-let ((files (eask-expand-file-specs (eask-args))))
      (progn
        (eask-pkg-init)
        (eask-ignore-errors
          (mapc #'load-file files)
          (ert-run-tests-batch)))
    (eask-info "(No tests found.)")
    (eask-help 'ert)))

;;; ert.el ends here

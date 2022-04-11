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

;; Handle options
(add-hook 'eask-before-command-hook
          (lambda ()
            (when (<= eask-verbosity 3) (setq ert-quiet t))))

(eask-start
  (if-let ((files (eask-expand-file-specs (eask-args))))
      (progn
        (eask-call "core/load")
        (ert-run-tests-batch))
    (eask-help 'ert)))

;;; ert.el ends here

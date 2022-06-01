;;; ert-runner.el --- Run ert tests using ert-runner  -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; Command to run ert tests using ert-runner,
;;
;;   $ eask ert-runner
;;

;;; Code:

(load (expand-file-name
       "../_prepare.el"
       (file-name-directory (nth 1 (member "-scriptload" command-line-args))))
      nil t)

;; Handle options
(add-hook 'eask-before-command-hook
          (lambda ()
            (when (= eask-verbosity 4) (setq ert-runner-verbose t))))

(eask-start
  (eask-with-archives "melpa"
    (eask-package-install 'ert-runner))
  (require 'ert-runner))

;;; ert-runner.el ends here

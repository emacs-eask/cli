;;; test/ert-runner.el --- Run ert tests using ert-runner  -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; Command to run ert tests using ert-runner,
;;
;;   $ eask ert-runner
;;

;;; Code:

(let ((dir (file-name-directory (nth 1 (member "-scriptload" command-line-args)))))
  (load (expand-file-name "_prepare.el"
                          (locate-dominating-file dir "_prepare.el"))
        nil t))

;; Handle options
(add-hook 'eask-before-command-hook
          (lambda ()
            (when (eask-reach-verbosity-p 'debug)
              (setq ert-runner-verbose t))))

(eask-start
  (eask-with-archives '("gnu" "melpa")
    (eask-package-install 'ert-runner))
  (require 'ert-runner))

;;; test/ert-runner.el ends here

;;; generate/test/ert-runner.el --- Create a new test project for the ert-runner  -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; Command use to create a new test project for the ert-runner,
;;
;;   $ eask generate test ert-runner
;;

;;; Code:

(let ((dir (file-name-directory (nth 1 (member "-scriptload" command-line-args)))))
  (load (expand-file-name "_prepare.el"
                          (locate-dominating-file dir "_prepare.el"))
        nil t))

(eask-start
  (eask-with-archives '("gnu" "melpa")
    (eask-package-install 'ert-runner))
  (advice-add 'ert-runner/run :override #'ignore)
  (load-library "ert-runner")
  (ert-runner/init (eask-guess-package-name)))

;;; generate/test/ert-runner.el ends here

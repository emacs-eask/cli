;;; generate/test/ert-runner.el --- Create a new test project for the ert-runner  -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; Command use to create a new test project for the ert-runner,
;;
;;   $ eask generate test ert-runner [names..]
;;
;;
;;  Positionals:
;;
;;    [names..]     specify test names
;;

;;; Code:

(let ((dir (file-name-directory (nth 1 (member "-scriptload" command-line-args)))))
  (load (expand-file-name "_prepare.el"
                          (locate-dominating-file dir "_prepare.el"))
        nil t))

(eask-load "generate/test/ert")

(defun eask-generate-test-ert-runner--test-helper (&optional name)
  "Generate test helper for NAME."
  (with-temp-file (f-join ert-runner-test-path "test-helper.el")
    (insert (format "\
;;; test-helper.el --- Helpers for %s

;;; test-helper.el ends here
" name))))

(eask-start
  (eask-archive-install-packages '("gnu" "melpa")
                                 'ert-runner)
  (advice-add 'ert-runner/run :override #'ignore)
  (load-library "ert-runner")
  (load-library "f")
  (let ((name (eask-guess-package-name)))
    (eask-generate-test-ert--init name)
    (eask-generate-test-ert-runner--test-helper name))
  (mapc #'eask-generate-test-ert--create-test-file (eask-args)))

;;; generate/test/ert-runner.el ends here

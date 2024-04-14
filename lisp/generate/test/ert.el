;;; generate/test/ert.el --- Create a new test project for the ert tests  -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; Command use to create a new test project for the ert tests,
;;
;;   $ eask generate test ert
;;

;;; Code:

(let ((dir (file-name-directory (nth 1 (member "-scriptload" command-line-args)))))
  (load (expand-file-name "_prepare.el"
                          (locate-dominating-file dir "_prepare.el"))
        nil t))

(defun eask-generate-test-ert--init (&optional name)
  "Create new test project (optional project name)."
  (let ((name (or name (f-filename default-directory)))
        (test-path (expand-file-name "test" default-directory)))
    (when (f-dir? "test")
      (error "%s" (ansi-red "Directory `test` already exists.")))
    (message "create %s" (ansi-green (f-filename test-path)))
    (f-mkdir test-path)
    (let ((test-file (s-concat name "-test.el")))
      (message "create  %s" (ansi-green (s-concat name "-test.el")))
      (with-temp-file (f-join test-path test-file)
        (insert (format "\
;;; %s --- Tests for %s

;;; %s ends here
" test-file name test-file))))))

(eask-start
  (eask-with-archives '("gnu" "melpa")
    (eask-package-install 'f))
  (require 'f)
  (eask-generate-test-ert--init (eask-guess-package-name)))

;;; generate/test/ert.el ends here

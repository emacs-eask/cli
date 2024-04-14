;;; generate/test/buttercup.el --- Create a new Buttercup setup for the project  -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; Command use to create a new Buttercup setup for the project,
;;
;;   $ eask generate test buttercup
;;

;;; Code:

(let ((dir (file-name-directory (nth 1 (member "-scriptload" command-line-args)))))
  (load (expand-file-name "_prepare.el"
                          (locate-dominating-file dir "_prepare.el"))
        nil t))

(defun eask-generate-test-buttercup--init (&optional name)
  "Create new test project (optional project name)."
  (let ((name (or name (f-filename default-directory)))
        (test-path (expand-file-name "tests" default-directory)))
    (when (f-dir? test-path)
      (error "%s" (ansi-red "Directory `tests` already exists.")))
    (message "create %s" (ansi-green (f-filename test-path)))
    (f-mkdir "tests")
    (let ((test-file (s-concat  "test-" name ".el")))
      (message "create  %s" (ansi-green test-file))
      (with-temp-file (f-join test-path test-file)
        (insert (format "\
;;; %s --- Buttercup tests for %s  -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(require 'buttercup)

;; Example test!
(describe \"A suite\"
  (it \"contains a spec with an expectation\"
    (expect t :to-be t)))

;;; %s ends here
" test-file name test-file))))))

(eask-start
  (eask-with-archives '("gnu" "melpa")
    (eask-package-install 'buttercup)
    (eask-package-install 'f))
  (require 'buttercup)
  (require 'f)
  (eask-generate-test-buttercup--init (eask-guess-package-name)))

;;; generate/test/buttercup.el ends here

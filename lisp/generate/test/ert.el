;;; generate/test/ert.el --- Create a new test project for the ert tests  -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; Command use to create a new test project for the ert tests,
;;
;;   $ eask generate test ert [names..]
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

(defvar eask-generate-test-ert-test-path
  (expand-file-name "test" default-directory)
  "The default test path.")

(defun eask-generate-test-ert--init (&optional name)
  "Create new test project (optional project NAME)."
  (let* ((name (or name (file-name-nondirectory default-directory)))
         (dir (file-directory-p eask-generate-test-ert-test-path)))
    (eask-with-progress
      (format "create %s folder... " (ansi-green "test"))
      (ignore-errors (make-directory eask-generate-test-ert-test-path t))
      (if dir "skipped ✗" "done ✓"))
    (eask-generate-test-ert--create-test-file name)))

(defun eask-generate-test-ert--create-test-file (name)
  "Generate test file by NAME."
  (let* ((test-file (concat name "-test.el"))
         (full-test-file (expand-file-name test-file eask-generate-test-ert-test-path))
         (ext (file-exists-p full-test-file)))
    (eask-with-progress
      (format "  create %s... " (ansi-green test-file))
      (with-temp-file full-test-file
        (insert (format "\
;;; %s --- Tests for %s

;;; %s ends here
" test-file name test-file)))
      (if ext "skipped ✗" "done ✓"))))

(eask-start
  (eask-generate-test-ert--init (eask-guess-package-name))
  (mapc #'eask-generate-test-ert--create-test-file (eask-args)))

;;; generate/test/ert.el ends here

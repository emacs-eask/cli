;;; lint/elisp-lint.el --- Run elisp-lint  -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; Commmand use to run `elisp-lint' for all files
;;
;;   $ eask lint elisp-lint [files..]
;;
;;
;;  Initialization options:
;;
;;    [files..]     files you want elisp-lint to run on
;;

;;; Code:

(load (expand-file-name
       "../_prepare.el"
       (file-name-directory (nth 1 (member "-scriptload" command-line-args))))
      nil t)

(defconst eask--elisp-lint-version nil
  "`elisp-lint' version.")

(defun eask--elisp-lint-process-file (filename)
  "Process FILENAME."
  (let* ((filename (expand-file-name filename))
         (file (eask-root-del filename))
         success)
    (eask-msg "")
    (eask-msg "`%s` with elisp-lint (%s)" (ansi-green file) eask--elisp-lint-version)
    (eask-with-verbosity 'debug
      (setq success (elisp-lint-file filename)))
    (when success
      (eask-msg "No issues found"))))

(eask-start
  (eask-with-archives "melpa"
    (eask-package-install 'elisp-lint))
  (setq eask--elisp-lint-version (eask-package--version-string 'elisp-lint))
  (require 'elisp-lint)
  (if-let ((files (eask-args-or-package-el-files)))
      (progn
        (mapcar #'eask--elisp-lint-process-file files)
        (eask-info "(Total of %s files linted)" (length files)))
    (eask-info "(No files have been linted)")
    (if (eask-args)
        (eask--print-no-matching-files)
      (eask-help "lint/elisp-lint"))))

;;; lint/elisp-lint.el ends here

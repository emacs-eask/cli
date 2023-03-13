;;; lint/elisp-lint.el --- Run elisp-lint  -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; Commmand use to run `elisp-lint' for all files
;;
;;   $ eask lint elisp-lint [files..]
;;
;;
;;  Positional arguments:
;;
;;    [files..]     files you want elisp-lint to run on
;;

;;; Code:

(let ((dir (file-name-directory (nth 1 (member "-scriptload" command-line-args)))))
  (load (expand-file-name "_prepare.el"
                          (locate-dominating-file dir "_prepare.el"))
        nil t))

;;
;;; Flags

(advice-add #'eask-allow-error-p :override (lambda (&rest _) t))

;;
;;; Core

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
    ;; Report result!
    (cond (success
           (eask-msg "No issues found"))
          ((eask-strict-p)
           (eask-error "Linting failed")))))

(eask-start
  ;; Preparation
  (eask-with-archives "melpa"
    (eask-package-install 'elisp-lint))
  (setq eask--elisp-lint-version (eask-package--version-string 'elisp-lint))

  ;; Start Linting
  (require 'elisp-lint)
  (let* ((patterns (eask-args))
         (files (if patterns
                    (eask-expand-file-specs patterns)
                  (eask-package-el-files))))
    (cond
     ;; Files found, do the action!
     (files
      (mapcar #'eask--elisp-lint-process-file files)
      (eask-msg "")
      (eask-info "(Total of %s file%s linted)" (length files)
                 (eask--sinr files "" "s")))
     ;; Pattern defined, but no file found!
     (patterns
      (eask-msg "")
      (eask-info "(No files match wildcard: %s)"
                 (mapconcat #'identity patterns " ")))
     ;; Default, print help!
     (t
      (eask-msg "")
      (eask-info "(No files have been linted)")
      (eask-help "lint/elisp-lint")))))

;;; lint/elisp-lint.el ends here

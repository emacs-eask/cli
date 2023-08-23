;;; lint/checkdoc.el --- Run checkdoc  -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; Commmand use to run `checkdoc' for all files
;;
;;   $ eask lint checkdoc [files..]
;;
;;
;;  Positionals:
;;
;;    [files..]     files you want checkdoc to run on
;;

;;; Code:

(let ((dir (file-name-directory (nth 1 (member "-scriptload" command-line-args)))))
  (load (expand-file-name "_prepare.el"
                          (locate-dominating-file dir "_prepare.el"))
        nil t))

;;
;;; Externals

(defvar checkdoc-version)
(defvar checkdoc-create-error-function)

(declare-function checkdoc-buffer-label "ext:checkdoc.el")

;;
;;; Flags

(advice-add #'eask-allow-error-p :override (lambda (&rest _) t))

;;
;;; Core

(defvar eask--checkdoc-errors nil "Error flag.")

(defun eask--checkdoc-print-error (text start _end &optional _unfixable)
  "Print error for checkdoc.

Arguments TEXT, START, END and UNFIXABLE are required for this function to
be assigned to variable `checkdoc-create-error-function'."
  (setq eask--checkdoc-errors t)
  (let ((msg (concat (checkdoc-buffer-label) ":"
                     (int-to-string (count-lines (point-min) (or start (point-min))))
                     ": " text)))
    (if (eask-strict-p) (error msg) (warn msg))
    ;; Return nil because we *are* generating a buffered list of errors.
    nil))

(setq checkdoc-create-error-function #'eask--checkdoc-print-error)

(defun eask--checkdoc-file (filename)
  "Run checkdoc on FILENAME."
  (let* ((filename (expand-file-name filename))
         (file (eask-root-del filename))
         (eask--checkdoc-errors))
    (eask-lint-first-newline)
    (eask-msg "`%s` with checkdoc (%s)" (ansi-green file) checkdoc-version)
    (checkdoc-file filename)
    (unless eask--checkdoc-errors (eask-msg "No issues found"))))

(eask-start
  (require 'checkdoc)
  (let* ((patterns (eask-args))
         (files (if patterns
                    (eask-expand-file-specs patterns)
                  (eask-package-el-files))))
    (cond
     ;; Files found, do the action!
     (files
      (mapcar #'eask--checkdoc-file files)
      (eask-msg "")
      (eask-info "(Total of %s file%s %s checked)" (length files)
                 (eask--sinr files "" "s")
                 (eask--sinr files "has" "have")))
     ;; Pattern defined, but no file found!
     (patterns
      (eask-info "(No files match wildcard: %s)"
                 (mapconcat #'identity patterns " ")))
     ;; Default, print help!
     (t
      (eask-info "(No files have been linted)")
      (eask-help "lint/checkdoc")))))

;;; lint/checkdoc.el ends here

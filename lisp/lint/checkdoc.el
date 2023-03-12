;;; lint/checkdoc.el --- Run checkdoc  -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; Commmand use to run `checkdoc' for all files
;;
;;   $ eask lint checkdoc [files..]
;;
;;
;;  Positional arguments:
;;
;;    [files..]     files you want checkdoc to run on
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

(defvar eask--checkdoc-errors nil "Error flag.")

(defun eask--checkdoc-print-error (text start end &optional unfixable)
  "Print error for checkdoc."
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
    (eask-msg "")
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
      (eask-info "No files found with wildcard pattern: %s"
                 (mapconcat #'identity patterns " ")))
     ;; Default, print help!
     (t
      (eask-info "(No files have been linted)")
      (eask-help "lint/checkdoc")))))

;;; lint/checkdoc.el ends here

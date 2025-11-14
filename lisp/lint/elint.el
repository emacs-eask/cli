;;; lint/elint.el --- Run elint  -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; Commmand use to run `elint' for all files
;;
;;   $ eask lint elint [files..]
;;
;;
;;  Positionals:
;;
;;    [files..]     files you want elint to run on
;;

;;; Code:

(let ((dir (file-name-directory (nth 1 (member "-scriptload" command-line-args)))))
  (load (expand-file-name "_prepare.el"
                          (locate-dominating-file dir "_prepare.el"))
        nil t))

;;
;;; Externals

(declare-function elint-get-log-buffer "ext:elsa.el")

;;
;;; Flags

(advice-add #'eask-allow-error-p :override #'eask-always)

;;
;;; Core

(defvar eask-lint-elint--warnings-p nil
  "Non-nil if any warnings were reported in the run.")

(defun eask-lint-elint--file (filename)
  "Run elint on FILENAME."
  (let* ((filename (expand-file-name filename))
         (file (eask-root-del filename))
         (noninteractive))
    (eask-lint-first-newline)
    (eask-msg "`%s` with elint" (ansi-green file))
    (eask-with-verbosity 'debug (elint-file filename))
    (let ((log-buffer (elint-get-log-buffer)))
      (eask-print-log-buffer log-buffer)
      (with-current-buffer log-buffer
        (goto-char (point-min))
        (when (re-search-forward ":Warning:" nil t)
          (setq eask-lint-elint--warnings-p t)))
      (kill-buffer log-buffer))))

(defun eask-lint-elint--has-error-p ()
  "Return non-nil if we should report error for exit status."
  (and eask-lint-elint--warnings-p
       (eask-strict-p)))

(eask-start
  (require 'elint)
  (let* ((patterns (eask-args))
         (files (if patterns
                    (eask-expand-file-specs patterns)
                  (eask-package-el-files))))
    (cond
     ;; Files found, do the action!
     (files
      (mapcar #'eask-lint-elint--file files)
      (eask-msg "")
      (eask-info "(Total of %s file%s %s checked)" (length files)
                 (eask--sinr files "" "s")
                 (eask--sinr files "has" "have"))
      ;; Report error.
      (when (eask-lint-elint--has-error-p)
        (eask--exit 'failure)))
     ;; Pattern defined, but no file found!
     (patterns
      (eask-info "(No files match wildcard: %s)"
                 (mapconcat #'identity patterns " ")))
     ;; Default, print help!
     (t
      (eask-msg "")
      (eask-info "(No files have been linted)")
      (eask-help "lint/elint")))))

;;; lint/elint.el ends here

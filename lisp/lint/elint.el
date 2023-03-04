;;; lint/elint.el --- Run elint  -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; Commmand use to run `elint' for all files
;;
;;   $ eask lint elint [files..]
;;
;;
;;  Initialization options:
;;
;;    [files..]     files you want elint to run on
;;

;;; Code:

(load (expand-file-name
       "../_prepare.el"
       (file-name-directory (nth 1 (member "-scriptload" command-line-args))))
      nil t)

;;
;;; Flags

(advice-add #'eask-allow-error-p :override (lambda (&rest _) t))

;;
;;; Core

(defun eask--elint-file (filename)
  "Run elint on FILENAME."
  (let* ((filename (expand-file-name filename))
         (file (eask-root-del filename))
         (noninteractive))
    (eask-msg "")
    (eask-msg "`%s` with elint" (ansi-green file))
    (eask-with-verbosity 'debug (elint-file filename))
    (eask-print-log-buffer (elint-get-log-buffer))
    (kill-buffer (elint-get-log-buffer))))

(eask-start
  (require 'elint)
  (let* ((patterns (eask-args))
         (files (if patterns
                    (eask-expand-file-specs patterns)
                  (eask-package-el-files))))
    (cond
     ;; Files found, do the action!
     (files
      (mapcar #'eask--elint-file files)
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
      (eask-msg "")
      (eask-info "(No files have been checked (elint))")
      (eask-help "lint/elint")))))

;;; lint/elint.el ends here

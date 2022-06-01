;;; elint.el --- Run elint  -*- lexical-binding: t; -*-

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
  (if-let* ((files (eask-args-or-package-el-files))
            (len (length files))
            (s (eask--sinr len "" "s"))
            (have (eask--sinr len "has" "have")))
      (progn
        (mapcar #'eask--elint-file files)
        (eask-info "(Total of %s file%s %s checked)" len s have))
    (eask-info "(No files have been checked (elint))")
    (if (eask-args)
        (eask--print-no-matching-files)
      (eask-help 'elint))))

;;; elint.el ends here

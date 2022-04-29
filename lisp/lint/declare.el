;;; declare.el --- Run declare  -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; Commmand use to run `declare' for all files
;;
;;   $ eask declare
;;
;;
;;  Initialization options:
;;
;;    [files..]     files you want checkdoc to run on
;;

;;; Code:

(load (expand-file-name
       "../_prepare.el"
       (file-name-directory (nth 1 (member "-scriptload" command-line-args))))
      nil t)

(defun eask--check-declare-file (filename)
  "Run check-declare on FILENAME."
  (let* ((filename (expand-file-name filename))
         (file (eask-root-del filename))
         (errors))
    (eask-msg "")
    (eask-msg "`%s` with check-declare" (ansi-green file))
    (setq errors (check-declare-file filename))
    (if errors
        (with-current-buffer check-declare-warning-buffer
          (eask-msg (buffer-string)))
      (eask-msg "No issues found"))))

(eask-start
  (require 'check-declare)
  (if-let* ((files (if (eask-args)
                       (eask-expand-file-specs (eask-args))
                     (eask-package-el-files)))
            (len (length files))
            (s (eask--sinr len "" "s"))
            (have (eask--sinr len "has" "have")))
      (progn
        (mapcar #'eask--check-declare-file files)
        (eask-info "(Total of %s file%s %s checked)" len s have))
    (eask-info "(No files have been checked (declare))")
    (if (eask-args)
        (eask--print-no-matching-files)
      (eask-help 'declare))))

;;; declare.el ends here
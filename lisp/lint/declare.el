;;; lint/declare.el --- Run check-declare  -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; Commmand use to run `check-declare' for all files
;;
;;   $ eask lint declare [files..]
;;
;;
;;  Positionals:
;;
;;    [files..]     files you want check-declare to run on
;;

;;; Code:

(let ((dir (file-name-directory (nth 1 (member "-scriptload" command-line-args)))))
  (load (expand-file-name "_prepare.el"
                          (locate-dominating-file dir "_prepare.el"))
        nil t))

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
  (let* ((patterns (eask-args))
         (files (if patterns
                    (eask-expand-file-specs patterns)
                  (eask-package-el-files))))
    (cond
     ;; Files found, do the action!
     (files
      (mapcar #'eask--check-declare-file files)
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
      (eask-help "lint/declare")))))

;;; lint/declare.el ends here

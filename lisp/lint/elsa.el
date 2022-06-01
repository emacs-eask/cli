;;; elsa.el --- Run elsa  -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; Commmand use to run `elsa' for all files
;;
;;   $ eask lint elsa [files..]
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

(defconst eask--elsa-version nil
  "Elsa version.")

(defun eask--elsa-process-file (filename)
  "Process FILENAME."
  (let* ((filename (expand-file-name filename))
         (file (eask-root-del filename))
         errors)
    (eask-msg "")
    (eask-msg "`%s` with elsa (%s)" (ansi-green file) eask--elsa-version)
    (eask-with-verbosity 'debug
      (setq errors (oref (elsa-process-file filename) errors)))
    (if errors
        (--each (reverse errors)
          (let ((line (string-trim (concat file ":" (elsa-message-format it)))))
            (cond ((string-match-p "[: ][Ee]rror:" line) (eask-error line))
                  ((string-match-p "[: ][Ww]arning:" line) (eask-warn line))
                  (t (eask-log line)))))
      (eask-msg "No issues found"))))

(eask-start
  (eask-with-archives "melpa"
    (eask-package-install 'elsa))
  (setq eask--elsa-version (eask-package--version-string 'elsa))
  (require 'elsa)
  (if-let ((files (eask-args-or-package-el-files)))
      (progn
        (mapcar #'eask--elsa-process-file files)
        (eask-info "(Total of %s files linted)" (length files)))
    (eask-info "(No files have been linted)")
    (if (eask-args)
        (eask--print-no-matching-files)
      (eask-help 'elsa))))

;;; elsa.el ends here

;;; regexps.el --- Lint the package using `relint'  -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; Commmand use to run `relint' for all files
;;
;;   $ eask lint regexps [files..]
;;
;;
;;  Initialization options:
;;
;;    [files..]     files you want relint to run on
;;

;;; Code:

(load (expand-file-name
       "../_prepare.el"
       (file-name-directory (nth 1 (member "-scriptload" command-line-args))))
      nil t)

(defconst eask--relint-version nil
  "`relint' version.")

(defun eask--relint-file (filename)
  "Package lint FILENAME."
  (let* ((filename (expand-file-name filename))
         (file (eask-root-del filename))
         (errors))
    (eask-msg "")
    (eask-msg "`%s` with relint (%s)" (ansi-green file) eask--relint-version)
    (with-current-buffer (find-file filename)
      (setq errors (relint-buffer (current-buffer)))
      (dolist (err errors)
        (let* ((msg       (nth 0 err))
               (error-pos (nth 2 err))
               (severity  (nth 5 err))
               (report-func (pcase severity
                              (`error #'eask-error)
                              (`warning #'eask-warn))))
          (funcall report-func "%s:%s %s: %s"
                   file (line-number-at-pos error-pos)
                   (capitalize (eask-2str severity)) msg)))
      (kill-this-buffer))))

(eask-start
  (eask-with-archives "gnu"
    (eask-package-install 'relint))
  (setq eask--relint-version (eask-package--version-string 'relint))
  (require 'relint)
  (if-let ((files (eask-args-or-package-el-files)))
      (progn
        (setq package-lint-main-file eask-package-file)
        (mapcar #'eask--relint-file files)
        (eask-info "(Total of %s files linted)" (length files)))
    (eask-info "(No files have been linted)")
    (if (eask-args)
        (eask--print-no-matching-files)
      (eask-help 'regexps))))

;;; regexps.el ends here

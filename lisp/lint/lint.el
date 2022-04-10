;;; lint.el --- Lint the package using `package-lint'  -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; Command use to lint current Emacs package,
;;
;;   $ eask lint [names..]
;;
;;
;;  Initialization options:
;;
;;    [names..]     specify files to byte-compile
;;

;;; Code:

(load (expand-file-name
       "../_prepare.el"
       (file-name-directory (nth 1 (member "-scriptload" command-line-args))))
      nil t)

(defun eask--package-lint-file (filename)
  "Package lint FILENAME."
  (let* ((filename (expand-file-name filename))
         (file (eask-root-del filename)))
    (message "")
    (message "`%s` with package-lint" file)
    (with-temp-buffer
      (emacs-lisp-mode)
      (insert-file-contents filename)
      (package-lint-current-buffer)))
  (eask-print-log-buffer "*Package-Lint*"))

(eask-start
  (eask-with-archives "melpa"
    (eask-package-install 'package-lint))
  (if-let ((files (or (eask-args) (eask-package-el-files))))
      (progn
        (mapcar #'eask--package-lint-file files)
        (eask-info "(Total of %s files linted)" (length files)))
    (eask-info "(No files have been linted)")))

;;; lint.el ends here

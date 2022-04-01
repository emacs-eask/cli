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
       "_prepare.el"
       (file-name-directory (nth 1 (member "-scriptload" command-line-args))))
      nil t)

(defun eask--print-package-lint-buffer ()
  "Print `*Package-Lint*' buffer."
  (with-current-buffer "*Package-Lint*"
    (goto-char (point-min))
    (while (not (eobp))
      (let ((line (thing-at-point 'line)))
        (cond ((string-match-p " error: " line) (eask-error line))
              ((string-match-p " warning: " line) (eask-warn line))
              (t (eask-log line))))
      (forward-line 1))))

(defun eask--package-lint-file (filename)
  "Package lint FILENAME."
  (let* ((filename (expand-file-name filename))
         (file (s-replace eask-file-root "" filename)))
    (message "")
    (message "`%s` with package-lint" file)
    (with-temp-buffer
      (emacs-lisp-mode)
      (insert-file-contents filename)
      (package-lint-current-buffer)))
  (eask--print-package-lint-buffer))

(eask-start
  (eask-package-install 'package-lint)
  (if-let ((files (or (eask-args) (eask-package-el-files))))
      (progn
        (mapcar #'eask--package-lint-file files)
        (eask-info "(Total of %s files linted)" (length files)))
    (eask-info "(No files have been linted)")))

;;; lint.el ends here

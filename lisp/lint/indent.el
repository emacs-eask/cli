;;; indent.el --- Lint the package using `indent-lint'  -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; Command use to ,
;;
;;   $ eask indent [names..]
;;
;;
;;  Initialization options:
;;
;;    [names..]     specify files to do indent lint
;;

;;; Code:

(load (expand-file-name
       "../_prepare.el"
       (file-name-directory (nth 1 (member "-scriptload" command-line-args))))
      nil t)

(defun eask--undo-lines (undo-list)
  "Return list of lines changed in UNDO-LIST."
  (let ((lines))
    (dolist (elm undo-list)
      (when (and (consp elm) (numberp (cdr elm)))
        (push (line-number-at-pos (abs (cdr elm))) lines)))
    (reverse lines)))

(defun eask--indent-lint-file (file)
  "Lint indent for FILE."
  (eask-msg "")
  (eask-msg "`%s` with indent-lint" (ansi-green (eask-root-del file)))
  (find-file file)
  (let ((report-func (if (eask-strict-p) #'eask-error #'eask-warn))
        (tick (buffer-modified-tick)))
    (eask--silent (indent-region (point-min) (point-max)))
    (if (/= tick (buffer-modified-tick))
        ;; Indentation changed: warn for each line.
        (dolist (line (eask--undo-lines buffer-undo-list))
          (funcall report-func "%s:%s: Indentation mismatch" (buffer-name) line))
      (eask-msg "No issues found"))))

(eask-start
  (if-let ((files (if (eask-args)
                      (eask-expand-file-specs (eask-args))
                    (eask-package-el-files))))
      (progn
        (mapcar #'eask--indent-lint-file files)
        (eask-info "(Total of %s files linted)" (length files)))
    (eask-info "(No files have been linted)")
    (if (eask-args)
        (eask--print-no-matching-files)
      (eask-help 'indent))))

;;; indent.el ends here

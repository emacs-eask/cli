;;; lint/indent.el --- Lint the package using `indent-lint'  -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; Command use to check package with indent-lint,
;;
;;   $ eask lint indent [files..]
;;
;;
;;  Positionals:
;;
;;    [files..]     files you want indent-lint to run on
;;

;;; Code:

(let ((dir (file-name-directory (nth 1 (member "-scriptload" command-line-args)))))
  (load (expand-file-name "_prepare.el"
                          (locate-dominating-file dir "_prepare.el"))
        nil t))

;;
;;; Core

(defun eask-lint-indent--undo-pos (entry)
  "Return the undo pos from ENTRY."
  (cl-typecase (car entry)
    (number (car entry))
    (string (abs (cdr entry)))))

(defun eask-lint-indent--undo-infos (undo-list)
  "Return list of infos in UNDO-LIST."
  (let ((infos))
    (dolist (elm undo-list)
      (when-let* ((pos (eask-lint-indent--undo-pos elm))
                  (line (line-number-at-pos pos))
                  (expected (progn (eask--goto-line line)
                                   (current-indentation))))
        (push (list line expected) infos)))
    infos))

(defun eask-lint-indent--file (file)
  "Lint indent for FILE."
  (eask-msg "")
  (eask-msg "`%s` with indent-lint" (ansi-green (eask-root-del file)))
  (find-file file)
  (let ((tick (buffer-modified-tick))
        (bs (buffer-string)))
    (eask-with-temp-buffer (insert bs))
    (eask--silent (indent-region (point-min) (point-max)))
    (if-let* (((/= tick (buffer-modified-tick)))
              (infos (eask-lint-indent--undo-infos buffer-undo-list)))
        ;; Indentation changed: warn for each line.
        (dolist (info infos)
          (let* ((line    (nth 0 info))
                 (column  (nth 1 info))
                 (current (eask-with-buffer
                            (eask--goto-line line) (current-indentation))))
            (eask-ignore-errors
              (eask-report "%s:%s: Expected indentation is %s but found %s"
                           (buffer-name) line column current))))
      (eask-log "No mismatch indentation found"))))

(eask-start
  (let* ((patterns (eask-args))
         (files (if patterns (eask-expand-file-specs (eask-args))
                  (eask-package-el-files))))
    ;; XXX: Load all dependencies and elisp files to ensure
    ;; all macros' indentation is applied.
    (progn
      (eask-install-dependencies)
      (eask-with-verbosity 'debug
        (ignore-errors (mapc #'load (eask-package-el-files)))))
    (cond
     ;; Files found, do the action!
     (files
      (mapcar #'eask-lint-indent--file files)
      (eask-msg "")
      (eask-info "(Total of %s file%s linted)" (length files)
                 (eask--sinr files "" "s")))
     ;; Pattern defined, but no file found!
     (patterns
      (eask-info "(No files match wildcard: %s)"
                 (mapconcat #'identity patterns " ")))
     ;; Default, print help!
     (t
      (eask-info "(No files have been linted)")
      (eask-help "lint/indent")))))

;;; lint/indent.el ends here

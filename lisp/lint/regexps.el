;;; lint/regexps.el --- Lint the package using `relint'  -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; Commmand use to run `relint' for all files
;;
;;   $ eask lint regexps [files..]
;;
;;
;;  Positionals:
;;
;;    [files..]     files you want relint to run on
;;

;;; Code:

(let ((dir (file-name-directory (nth 1 (member "-scriptload" command-line-args)))))
  (load (expand-file-name "_prepare.el"
                          (locate-dominating-file dir "_prepare.el"))
        nil t))

;;
;;; Flags

(advice-add #'eask-allow-error-p :override (lambda (&rest _) t))

;;
;;; Core

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
      (unless errors
        (eask-msg "No issues found"))
      (kill-this-buffer))))

(eask-start
  ;; Preparation
  (eask-with-archives "gnu"
    (eask-package-install 'relint))
  (setq eask--relint-version (eask-package--version-string 'relint))

  ;; Start Linting
  (require 'relint)
  (let* ((patterns (eask-args))
         (files (if patterns
                    (eask-expand-file-specs patterns)
                  (eask-package-el-files))))
    (cond
     ;; Files found, do the action!
     (files
      (setq package-lint-main-file eask-package-file)
      (mapcar #'eask--relint-file files)
      (eask-msg "")
      (eask-info "(Total of %s file%s linted)" (length files)
                 (eask--sinr files "" "s")))
     ;; Pattern defined, but no file found!
     (patterns
      (eask-msg "")
      (eask-info "(No files match wildcard: %s)"
                 (mapconcat #'identity patterns " ")))
     ;; Default, print help!
     (t
      (eask-msg "")
      (eask-info "(No files have been linted)")
      (eask-help "lint/regexps")))))

;;; lint/regexps.el ends here

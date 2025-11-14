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
;;; Externals

(declare-function relint-buffer "ext:package-lint.el")

;;
;;; Flags

(eask-command-check "27.1")

(advice-add #'eask-allow-error-p :override #'eask-always)

;;
;;; Core

(defconst eask-lint-regexps--relint-version nil
  "`relint' version.")

(defvar eask-lint-regexps--warnings-p nil
  "Non-nil if any warnings were reported in the run.")

(defun eask-lint-regexps--relint-file (filename)
  "Package lint FILENAME."
  (let* ((filename (expand-file-name filename))
         (file (eask-root-del filename))
         (errors))
    (eask-msg "")
    (eask-msg "`%s` with relint (%s)" (ansi-green file) eask-lint-regexps--relint-version)
    (with-current-buffer (find-file filename)
      (setq errors (relint-buffer (current-buffer)))
      (dolist (err errors)
        (let* ((msg       (seq-elt err 0))
               (error-pos (seq-elt err 2))
               (severity  (seq-elt err 7))
               (report-func (pcase severity
                              (`error   #'eask-error)
                              (`warning #'eask-warn)
                              (_        #'eask-info))))
          (when (eq severity 'warning)
            (setq eask-lint-regexps--warnings-p t))
          (funcall report-func "%s:%s %s: %s"
                   file (line-number-at-pos error-pos)
                   (capitalize (eask-2str severity)) msg)))
      (unless errors
        (eask-msg "No issues found"))
      (kill-current-buffer))))

(defun eask-lint-regexps--has-error-p ()
  "Return non-nil if we should report error for exit status."
  (and eask-lint-regexps--warnings-p
       (eask-strict-p)))

(eask-start
  ;; Preparation
  (eask-archive-install-packages '("gnu")
                                 'relint)
  (setq eask-lint-regexps--relint-version (eask-package--version-string 'relint))

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
      (mapcar #'eask-lint-regexps--relint-file files)
      (eask-msg "")
      (eask-info "(Total of %s file%s linted)" (length files)
                 (eask--sinr files "" "s"))
      ;; Report error.
      (when (eask-lint-regexps--has-error-p)
        (eask--exit 'failure)))
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

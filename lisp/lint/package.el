;;; lint/package.el --- Lint the package using `package-lint'  -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; Command use to lint current Emacs package,
;;
;;   $ eask lint package [files..]
;;
;;
;;  Positionals:
;;
;;    [files..]     specify files to do package lint
;;

;;; Code:

(let ((dir (file-name-directory (nth 1 (member "-scriptload" command-line-args)))))
  (load (expand-file-name "_prepare.el"
                          (locate-dominating-file dir "_prepare.el"))
        nil t))

;;
;;; Externals

(declare-function package-lint-current-buffer "ext:package-lint.el")

;;
;;; Flags

(advice-add #'eask-allow-error-p :override #'eask-always)

;;
;;; Handle options

(add-hook 'eask-before-command-hook
          (lambda ()
            (when (and (not (boundp 'package-lint-batch-fail-on-warnings))
                       (not (eask-strict-p)))
              ;; TODO: This doesn't work since this variable only controls
              ;; `package-lint' exit code.
              (setq package-lint-batch-fail-on-warnings nil))))

;;
;;; Core

(defvar eask-lint-package--warnings-p nil
  "Non-nil if any warnings were reported in the run.")

(defconst eask-lint-package--version nil
  "`package-lint' version.")

(defun eask-lint-package--file (filename)
  "Package lint FILENAME."
  (let* ((filename (expand-file-name filename))
         (file (eask-root-del filename)))
    (eask-msg "")
    (eask-msg "`%s` with package-lint (%s)" (ansi-green file) eask-lint-package--version)
    (with-current-buffer (find-file filename)
      (package-lint-current-buffer)
      (kill-current-buffer)))
  (with-current-buffer "*Package-Lint*"
      (goto-char (point-min))
      (when (re-search-forward "warning:" nil t)
        (setq eask-lint-package--warnings-p t)))
  (eask-print-log-buffer "*Package-Lint*"))

(eask-start
  ;; Preparation
  (eask-archive-install-packages '("gnu" "melpa")
                                 'package-lint)
  (setq eask-lint-package--version (eask-package--version-string 'package-lint))

  ;; Start Linting
  (require 'package-lint)
  (let* ((patterns (eask-args))
         (files (if patterns
                    (eask-expand-file-specs patterns)
                  (eask-package-el-files))))
    (cond
     ;; Files found, do the action!
     (files
      (eask-pkg-init)  ; XXX: Avoid not installable error!
      (setq package-lint-main-file eask-package-file)
      (mapcar #'eask-lint-package--file files)
      (eask-msg "")
      (eask-info "(Total of %s file%s linted)" (length files)
                 (eask--sinr files "" "s"))
      (when (and eask-lint-package--warnings-p
                 (eask-strict-p))
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
      (eask-help "lint/package")))))

;;; lint/package.el ends here

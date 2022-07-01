;;; package.el --- Lint the package using `package-lint'  -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; Command use to lint current Emacs package,
;;
;;   $ eask lint package [files..]
;;
;;
;;  Initialization options:
;;
;;    [files..]     specify files to do package lint
;;

;;; Code:

(load (expand-file-name
       "../_prepare.el"
       (file-name-directory (nth 1 (member "-scriptload" command-line-args))))
      nil t)

;; Handle options
(add-hook 'eask-before-command-hook
          (lambda ()
            (setq package-lint-batch-fail-on-warnings t)))

(defconst eask--package-lint-version nil
  "`package-lint' version.")

(defun eask--package-lint-file (filename)
  "Package lint FILENAME."
  (let* ((filename (expand-file-name filename))
         (file (eask-root-del filename)))
    (eask-msg "")
    (eask-msg "`%s` with package-lint (%s)" (ansi-green file) eask--package-lint-version)
    (with-current-buffer (find-file filename)
      (package-lint-current-buffer)
      (kill-this-buffer)))
  (eask-print-log-buffer "*Package-Lint*"))

(eask-start
  (eask-with-archives "melpa"
    (eask-package-install 'package-lint))
  (setq eask--package-lint-version (eask-package--version-string 'package-lint))
  (require 'package-lint)
  (if-let ((files (eask-args-or-package-el-files)))
      (progn
        (eask-pkg-init)
        (setq package-lint-main-file eask-package-file)
        (mapcar #'eask--package-lint-file files)
        (eask-info "(Total of %s files linted)" (length files)))
    (eask-info "(No files have been linted)")
    (if (eask-args)
        (eask--print-no-matching-files)
      (eask-help "lint/package"))))

;;; package.el ends here

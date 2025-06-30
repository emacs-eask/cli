;;; lint/org.el --- Run org-lint on Org files  -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; Commmand use to run `org-lint' for all files
;;
;;   $ eask lint org [files..]
;;
;;
;;  Positionals:
;;
;;    [files..]     files you want org-lint to run on
;;

;;; Code:

(let ((dir (file-name-directory (nth 1 (member "-scriptload" command-line-args)))))
  (load (expand-file-name "_prepare.el"
                          (locate-dominating-file dir "_prepare.el"))
        nil t))

;;
;;; Flags

(advice-add #'eask-allow-error-p :override #'eask-always)

;;
;;; Core

(defun eask-lint-org--print-error (file result)
  "Print the error RESULT from FILE."
  (let* ((data (cl-second result))
         (filename (file-name-nondirectory file))
         (line (elt data 0))
         (text (elt data 2))
         (msg (concat filename ":" line ": " text)))
    (if (eask-strict-p) (error msg) (warn msg))))

(defun eask-lint-org--file (file)
  "Run `org-lint' on FILE."
  (eask-msg "`%s` with org-lint" (ansi-green file))
  (with-temp-buffer
    (insert-file-contents file)
    (org-mode)
    (if-let* ((results (org-lint)))
        (mapc (lambda (result)
                (eask-lint-org--print-error file result))
              results)
      (eask-msg "No issues found"))))

(eask-start
  ;; Preparation
  (eask-archive-install-packages '("gnu")
                                 'org)

  ;; Start Linting
  (require 'org-lint)
  (let* ((patterns (eask-args))
         (files (eask-expand-file-specs patterns)))
    (cond
     ;; Files found, do the action!
     (files
      (mapcar #'eask-lint-org--file files)
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
      (eask-help "lint/org")))))

;;; lint/org.el ends here

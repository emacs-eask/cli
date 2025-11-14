;;; lint/elsa.el --- Run elsa  -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; Commmand use to run `elsa' for all files
;;
;;   $ eask lint elsa [files..]
;;
;;
;;  Positionals:
;;
;;    [files..]     files you want elsa to run on
;;

;;; Code:

(let ((dir (file-name-directory (nth 1 (member "-scriptload" command-line-args)))))
  (load (expand-file-name "_prepare.el"
                          (locate-dominating-file dir "_prepare.el"))
        nil t))

;;
;;; Externals

(require 'dash nil t)

(defvar elsa-global-state)

(declare-function elsa-message-format "ext:elsa.el")
(declare-function elsa-analyse-file "ext:elsa.el")
(declare-function --each "ext:dash.el")

;;
;;; Flags

(advice-add #'eask-allow-error-p :override #'eask-always)

;;
;;; Core

(defconst eask-lint-elsa--version nil
  "Elsa version.")

(defun eask-lint-elsa--analyse-file (filename)
  "Process FILENAME."
  (let* ((filename (expand-file-name filename))
         (file (eask-root-del filename))
         errors)
    (eask-msg "")
    (eask-msg "`%s` with elsa (%s)" (ansi-green file) eask-lint-elsa--version)
    (eask-with-verbosity 'debug
      (setq errors (oref (elsa-analyse-file filename elsa-global-state) errors)))
    (if errors
        (--each (reverse errors)
          (let ((line (string-trim (concat file ":" (elsa-message-format it)))))
            (cond ((string-match-p "[: ][Ee]rror:" line)
                   (eask-error line))
                  ((string-match-p "[: ][Ww]arning:" line)
                   (eask-warn line))
                  (t (eask-log line)))))
      (eask-msg "No issues found"))))

(defun eask-lint-elsa--has-error-p ()
  "Return non-nil if we should report error for exit status."
  (or eask--has-error-p
      (and eask--has-warn-p
           (eask-strict-p))))

(eask-start
  ;; Preparation
  (eask-archive-install-packages '("gnu" "melpa")
                                 'elsa)
  (setq eask-lint-elsa--version (eask-package--version-string 'elsa))

  ;; Start Linting
  (require 'elsa)
  (let* ((patterns (eask-args))
         (files (if patterns
                    (eask-expand-file-specs patterns)
                  (eask-package-el-files))))
    (cond
     ;; Files found, do the action!
     (files
      (elsa-load-config)
      (mapcar #'eask-lint-elsa--analyse-file files)
      (eask-msg "")
      (eask-info "(Total of %s file%s linted)" (length files)
                 (eask--sinr files "" "s"))
      ;; Report error.
      (when (eask-lint-elsa--has-error-p)
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
      (eask-help "lint/elsa")))))

;;; lint/elsa.el ends here

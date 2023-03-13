;;; lint/elsa.el --- Run elsa  -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; Commmand use to run `elsa' for all files
;;
;;   $ eask lint elsa [files..]
;;
;;
;;  Positional arguments:
;;
;;    [files..]     files you want elsa to run on
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

(defconst eask--elsa-version nil
  "Elsa version.")

(defun eask--elsa-analyse-file (filename)
  "Process FILENAME."
  (let* ((filename (expand-file-name filename))
         (file (eask-root-del filename))
         errors)
    (eask-msg "")
    (eask-msg "`%s` with elsa (%s)" (ansi-green file) eask--elsa-version)
    (eask-with-verbosity 'debug
      (setq errors (oref (elsa-analyse-file filename elsa-global-state) errors)))
    (if errors
        (--each (reverse errors)
          (let ((line (string-trim (concat file ":" (elsa-message-format it)))))
            (cond ((string-match-p "[: ][Ee]rror:" line) (eask-error line))
                  ((string-match-p "[: ][Ww]arning:" line) (eask-warn line))
                  (t (eask-log line)))))
      (eask-msg "No issues found"))))

(eask-start
  ;; Preparation
  (eask-with-archives "melpa"
    (eask-package-install 'elsa))
  (setq eask--elsa-version (eask-package--version-string 'elsa))

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
      (mapcar #'eask--elsa-analyse-file files)
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
      (eask-help "lint/elsa")))))

;;; lint/elsa.el ends here

;;; test/ecukes.el --- Run ecukes tests  -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; Command to run ecukes tests,
;;
;;   $ eask ecukes
;;
;;
;;  Positionals:
;;
;;    [files..]     specify feature files to do ecukes tests
;;

;;; Code:

(let ((dir (file-name-directory (nth 1 (member "-scriptload" command-line-args)))))
  (load (expand-file-name "_prepare.el"
                          (locate-dominating-file dir "_prepare.el"))
        nil t))

(defun eask-test-ecukes--run (files)
  "Run ecukes on FILES.

Modified from function `ecukes-cli/run'."
  (ecukes-load)
  (ecukes-reporter-use ecukes-cli-reporter)
  (ecukes-run files))

(eask-start
  ;; Preparation
  (eask-with-archives '("gnu" "melpa")
    (eask-package-install 'ecukes))

  ;; Start Testing
  (require 'ecukes)
  (let* ((patterns (eask-args))
         (files (eask-expand-file-specs patterns)))
    (cond
     ;; Files found, do the action!
     (files
      (eask-test-ecukes--run files))
     ;; Pattern defined, but no file found!
     (patterns
      (eask-msg "")
      (eask-info "(No files match wildcard: %s)"
                 (mapconcat #'identity patterns " ")))
     ;; Run default action.
     (t
      (ecukes)))))

;;; test/ecukes.el ends here

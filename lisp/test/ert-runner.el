;;; test/ert-runner.el --- Run ert tests using ert-runner  -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; Command to run ert tests using ert-runner,
;;
;;   $ eask test ert-runner [files..]
;;
;;
;;  Positionals:
;;
;;    [files..]     specify files to run ert tests
;;

;;; Code:

(let ((dir (file-name-directory (nth 1 (member "-scriptload" command-line-args)))))
  (load (expand-file-name "_prepare.el"
                          (locate-dominating-file dir "_prepare.el"))
        nil t))

;; Handle options
(add-hook 'eask-before-command-hook
          (lambda ()
            (when (eask-reach-verbosity-p 'debug)
              (setq ert-runner-verbose t))))

(defun eask-test-ert-runner--run (fnc &rest args)
  "Run around function `ert-runner/run'.

Arguments FNC and ARGS are used for advice `:around'.

Handle the argument ARGS when command arguments are specified."
  (let* ((patterns (eask-args))
         (files (eask-expand-file-specs patterns)))
    (setq args files))
  (apply fnc args))

(advice-add 'ert-runner/run :around #'eask-test-ert-runner--run)

(eask-start
  ;; Preparation
  (eask-archive-install-packages '("gnu" "melpa")
                                 'ert-runner)

  ;; Start Testing
  (require 'ert-runner))

;;; test/ert-runner.el ends here

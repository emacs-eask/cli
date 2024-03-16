;;; test/buttercup.el --- Run buttercup tests  -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; Command to run buttercup tests,
;;
;;   $ eask buttercup
;;

;;; Code:

(let ((dir (file-name-directory (nth 1 (member "-scriptload" command-line-args)))))
  (load (expand-file-name "_prepare.el"
                          (locate-dominating-file dir "_prepare.el"))
        nil t))

(eask-start
  ;; Preparation
  (eask-with-archives '("gnu" "melpa")
    (eask-package-install 'buttercup))

  ;; Start Testing
  (require 'buttercup)
  ;; Propose fix from https://github.com/jorgenschaefer/emacs-buttercup/pull/217
  (let ((load-path (cons "." load-path)))
    (buttercup-run-discover)))

;;; test/buttercup.el ends here

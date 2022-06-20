;;; buttercup.el --- Run buttercup tests  -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; Command to run buttercup tests,
;;
;;   $ eask buttercup
;;

;;; Code:

(load (expand-file-name
       "../_prepare.el"
       (file-name-directory (nth 1 (member "-scriptload" command-line-args))))
      nil t)

(eask-start
  (eask-with-archives "melpa"
    (eask-package-install 'buttercup))
  (require 'buttercup)
  ;; Propose fix from https://github.com/jorgenschaefer/emacs-buttercup/pull/217
  (let ((load-path (cons "." load-path)))
    (buttercup-run-discover)))

;;; buttercup.el ends here

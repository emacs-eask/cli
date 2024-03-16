;;; test/ecukes.el --- Run ecukes tests  -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; Command to run ecukes tests,
;;
;;   $ eask ecukes
;;

;;; Code:

(let ((dir (file-name-directory (nth 1 (member "-scriptload" command-line-args)))))
  (load (expand-file-name "_prepare.el"
                          (locate-dominating-file dir "_prepare.el"))
        nil t))

(eask-start
  ;; Preparation
  (eask-with-archives '("gnu" "melpa")
    (eask-package-install 'ecukes))

  ;; Start Testing
  (require 'ecukes)
  (ecukes))

;;; test/ecukes.el ends here

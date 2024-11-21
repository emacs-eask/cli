;;; generate/test/ecukes.el --- Create a new Ecukes setup for the project  -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; Command use to create a new Ecukes setup for the project,
;;
;;   $ eask generate test ecukes
;;

;;; Code:

(let ((dir (file-name-directory (nth 1 (member "-scriptload" command-line-args)))))
  (load (expand-file-name "_prepare.el"
                          (locate-dominating-file dir "_prepare.el"))
        nil t))

(eask-start
  (eask-archive-install-packages '("gnu" "melpa")
                                 'ecukes)
  (require 'ecukes-new)
  (ecukes-new))

;;; generate/test/ecukes.el ends here

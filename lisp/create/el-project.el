;;; create/el-project.el --- Create a new elisp project  -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; Create a new elisp project,
;;
;;   $ eask create el-project
;;

;;; Code:

(let ((dir (file-name-directory (nth 1 (member "-scriptload" command-line-args)))))
  (load (expand-file-name "_prepare.el"
                          (locate-dominating-file dir "_prepare.el"))
        nil t))

(eask-start
  ;; Preparation
  (eask-archive-install-packages '("gnu" "melpa" "jcs-elpa")
                                 'el-project)
  ;; Start project creation.
  (require 'el-project)
  (el-project-make-project))

;;; create/el-project.el ends here

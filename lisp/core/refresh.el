;;; core/refresh.el --- Download package archives  -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; Command use to download package archives
;;
;;   $ eask refresh
;;

;;; Code:

(let ((dir (file-name-directory (nth 1 (member "-scriptload" command-line-args)))))
  (load (expand-file-name "_prepare.el"
                          (locate-dominating-file dir "_prepare.el"))
        nil t))

(eask-start
  (advice-add 'package--download-one-archive :around #'eask--package-download-one-archive)
  (eask-pkg-init)
  (eask-msg "")
  (eask-info "(Done refresh package archives)"))

;;; core/refresh.el ends here

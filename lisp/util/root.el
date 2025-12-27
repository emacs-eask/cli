;;; util/root.el --- Display the effective installation directory  -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; Command use to display the effective installation directory,
;;
;;   $ eask root
;;

;;; Code:

(let ((dir (file-name-directory (nth 1 (member "-scriptload" command-line-args)))))
  (load (expand-file-name "_prepare.el"
                          (locate-dominating-file dir "_prepare.el"))
        nil t))

(eask-start
  (eask-println "%s\n" user-emacs-directory))

;;; util/root.el ends here

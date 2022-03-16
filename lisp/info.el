;;; info.el --- Print envrionemnt information  -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; Print the Emacs envrionemnt information
;;
;;   $ eask info
;;

;;; Code:

(load-file (expand-file-name
            "_prepare.el"
            (file-name-directory (nth 1 (member "-scriptload" command-line-args)))))

(defun eask--print-info (key)
  "Print package information."
  (when-let ((info (plist-get eask-package key)))
    (message "%s" info)))

(eask-start
  (eask--print-info :name)
  (eask--print-info :version)
  (eask--print-info :description))

;;; info.el ends here

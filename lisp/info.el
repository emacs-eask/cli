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

;; TODO: Are there any other information we want to print out?
(message "%s" (emacs-version))

(eask-start
  (message "%s" (plist-get eask-package :name))
  (message "%s" (plist-get eask-package :version))
  (message "%s" (plist-get eask-package :description)))

;;; info.el ends here

;;; init.el --- Install packages  -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; Create Eask file,
;;
;;   $ eask init
;;

;;; Code:

(load-file (expand-file-name
            "_prepare.el"
            (file-name-directory (nth 1 (member "-scriptload" command-line-args)))))

(eask-start
  (when-let ((name (read-string "Package name: " )))
    ;; TODO: ..
    ))

;;; init.el ends here

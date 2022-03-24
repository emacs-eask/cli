;;; archives.el --- List out all package archives  -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; Command use to list out all package archives,
;;
;;   $ eask archives
;;

;;; Code:

(load-file (expand-file-name
            "_prepare.el"
            (file-name-directory (nth 1 (member "-scriptload" command-line-args)))))

(defun eask--print-archive (archive)
  "Print the archive."
  (message "  %-15s %-40s" (car archive) (cdr archive)))

(eask-start
  (if package-archives
      (progn
        (message "")
        (mapc #'eask--print-archive package-archives))
    (message "\n No archive has been selected")))

;;; archives.el ends here

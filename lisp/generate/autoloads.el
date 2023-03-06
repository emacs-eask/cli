;;; generate/autoloads.el --- Generate autoload file  -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; Command use generate autoload file,
;;
;;   $ eask generate autoloads
;;

;;; Code:

(load (expand-file-name
       "../_prepare.el"
       (file-name-directory (nth 1 (member "-scriptload" command-line-args))))
      nil t)

(eask-start
  (let* ((name (eask-guess-package-name))
         (autoloads-file (expand-file-name (concat name "-autoloads.el"))))
    (package-generate-autoloads name default-directory)
    (eask-msg "")
    (eask-info "Write file %s..." autoloads-file)))

;;; generate/autoloads.el ends here

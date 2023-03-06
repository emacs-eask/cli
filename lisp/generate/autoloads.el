;;; generate/autoloads.el --- Generate autoload file  -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; Command use generate autoload file,
;;
;;   $ eask generate autoloads
;;

;;; Code:

(let ((dir (file-name-directory (nth 1 (member "-scriptload" command-line-args)))))
  (load (expand-file-name "_prepare.el"
                          (locate-dominating-file dir "_prepare.el"))
        nil t))

(eask-start
  (let* ((name (eask-guess-package-name))
         (autoloads-file (expand-file-name (concat name "-autoloads.el"))))
    (package-generate-autoloads name default-directory)
    (eask-msg "")
    (eask-info "Write file %s..." autoloads-file)))

;;; generate/autoloads.el ends here

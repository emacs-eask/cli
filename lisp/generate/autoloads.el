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
         (pkg-dir (or eask-package-file
                      default-directory))
         (pkg-dir (file-name-directory eask-package-file))
         (autoloads-file (expand-file-name (concat name "-autoloads.el") pkg-dir)))
    (package-generate-autoloads name pkg-dir)
    (eask-msg "")
    (eask-info "(Generated -autoloads.el file in %s)" autoloads-file)))

;;; generate/autoloads.el ends here

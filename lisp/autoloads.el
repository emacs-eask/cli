;;; autoloads.el --- Generate autoload file  -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; Command use generate autoload file,
;;
;;   $ eask autoloads
;;

;;; Code:

(load-file (expand-file-name
            "_prepare.el"
            (file-name-directory (nth 1 (member "-scriptload" command-line-args)))))

(eask-start
  (package-generate-autoloads
   (or (eask-package-get :name) (file-name-sans-extension eask-package-file))
   default-directory))

;;; autoloads.el ends here

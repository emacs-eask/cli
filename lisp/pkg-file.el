;;; pkg-file.el --- Generate -pkg file  -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; Command use generate -pkg file,
;;
;;   $ eask pkg-file
;;

;;; Code:

(load-file (expand-file-name
            "_prepare.el"
            (file-name-directory (nth 1 (member "-scriptload" command-line-args)))))

(eask-start
  (let* ((name (eask-guess-package-name))
         (version (eask-package-get :version))
         (description (eask-package-get :description))
         (pkg-file (expand-file-name (concat name "-pkg.el"))))
    (write-region
     (pp-to-string `(define-package ,name ,version ,description))
     nil pkg-file)
    (message "\nWrite file %s..." pkg-file)))

;;; pkg-file.el ends here

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
  (if (file-exists-p eask-file)
      (progn
        (message "Eask file is already exists")
        (message "\n%s\n" (with-temp-buffer (insert-file-contents eask-file) (buffer-string))))
    (let ((name (eask-argv 0))
          (version (eask-argv 1))
          (description (eask-argv 2))
          (pkg-file (eask-argv 3)))
      (write-region
       (format
        "(source \"gnu\")

(package \"%s\" \"%s\" \"%s\")

(package-file \"%s\")"
        name version description pkg-file)
       nil eask-file))))

;;; init.el ends here

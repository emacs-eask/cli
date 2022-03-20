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
  (eask-call "install")  ; XXX maybe try to avoid this?

  (let* ((name (or (eask-package-get :name)
                   (file-name-nondirectory (file-name-sans-extension eask-package-file))))
         (dir (file-name-directory (locate-library name)))
         (pkg-file (concat name "-pkg.el"))
         (gen-filename (concat dir pkg-file)))
    (write-region (with-temp-buffer
                    (insert-file-contents gen-filename)
                    (buffer-string))
                  nil pkg-file)
    (message "")
    (message "Write file %s..." gen-filename)))

;;; pkg-file.el ends here

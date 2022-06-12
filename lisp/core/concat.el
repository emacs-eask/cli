;;; concat.el --- Byte compile all Emacs Lisp files in the package  -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; Byte compile all Emacs Lisp files in the package
;;
;;   $ eask concat [names..]
;;
;;
;;  Initialization options:
;;
;;    [names..]         specify files to concatenate
;;
;;  Action options:
;;
;;    [destination]     optional output destination
;;

;;; Code:

(load (expand-file-name
       "../_prepare.el"
       (file-name-directory (nth 1 (member "-scriptload" command-line-args))))
      nil t)

(eask-start
  (let* ((name (eask-guess-package-name))
         (files (or (eask-expand-file-specs (eask-args))
                    (eask-package-el-files)))
         (eask-dist-path (or (eask-dest) eask-dist-path))
         (eask-dist-path (expand-file-name eask-dist-path))
         (target-file (concat name ".built.el"))
         (target-filename (expand-file-name target-file eask-dist-path)))
    (eask-debug "Destination path in %s" eask-dist-path)
    (ignore-errors (make-directory eask-dist-path t))

    (eask-info "Prepare to concatenate files %s..." target-filename)
    (write-region "" nil target-filename)

    (eask-with-verbosity 'log
      (with-temp-buffer
        (eask-progress-seq "  - Visiting" files "appended! ✓" #'insert-file-contents)
        (write-region (buffer-string) nil target-filename)
        (eask-info "Done. (Wrote file in %s)" target-filename)))))

;;; concat.el ends here

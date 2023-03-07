;;; core/concat.el --- Byte compile all Emacs Lisp files in the package  -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; Byte compile all Emacs Lisp files in the package
;;
;;   $ eask concat [names..]
;;
;;
;;  Positional arguments:
;;
;;    [names..]         specify files to concatenate
;;
;;  Action options:
;;
;;    [destination]     optional output destination
;;    [output]          optional output filename
;;

;;; Code:

(let ((dir (file-name-directory (nth 1 (member "-scriptload" command-line-args)))))
  (load (expand-file-name "_prepare.el"
                          (locate-dominating-file dir "_prepare.el"))
        nil t))

(eask-start
  (let* ((name (eask-guess-package-name))
         (patterns (eask-args))
         (files (if patterns (eask-expand-file-specs patterns)
                  (eask-package-el-files)))
         (eask-dist-path (or (eask-dest) eask-dist-path))
         (eask-dist-path (expand-file-name eask-dist-path))
         (target-file (concat name ".built.el"))
         (target-filename (or (and (eask-output) (expand-file-name (eask-output)))
                              (expand-file-name target-file eask-dist-path))))
    (cond
     ;; Files found, do the action!
     (files
      (eask-debug "Destination path in %s" eask-dist-path)
      (ignore-errors (make-directory eask-dist-path t))

      (eask-info "Prepare to concatenate files %s..." target-filename)
      (write-region "" nil target-filename)

      (eask-with-verbosity 'log
        (with-temp-buffer
          (eask-progress-seq "  - Visiting" files "appended! ✓" #'insert-file-contents)
          (write-region (buffer-string) nil target-filename)
          (eask-msg "")
          (eask-info "Done. (Wrote file in %s)" target-filename))))
     ;; Pattern defined, but no file found!
     (patterns
      (eask-info "No files found with wildcard pattern: %s"
                 (mapconcat #'identity patterns " ")))
     ;; Default, print help!
     (t
      (eask-info "(No files have been concatenated)")
      (eask-help "core/concat")))))

;;; core/concat.el ends here

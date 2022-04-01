;;; compile.el --- Byte compile all Emacs Lisp files in the package  -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; Byte compile all Emacs Lisp files in the package
;;
;;   $ eask compile [names..]
;;
;;
;;  Initialization options:
;;
;;    [names..]     specify files to byte-compile
;;

;;; Code:

(load (expand-file-name
       "_prepare.el"
       (file-name-directory (nth 1 (member "-scriptload" command-line-args))))
      nil t)

(defun eask--help-compile ()
  "Print help if command failed"
  (message "")
  (message "You need to specify file(s) you want to compile")
  (message "")
  (message "  $ eask %s FILE-1 FILE-2" (eask-command))
  (message "")
  (message "Or edit Eask file with [files] specifier")
  (message "")
  (message " [+] (files \"FILE-1\" \"FILE-2\")"))

;; Handle options
(when (eask-strict-p) (setq byte-compile-error-on-warn t))
(when (= eask-verbosity 4) (setq byte-compile-verbose t))

(defun eask--byte-compile-file (filename)
  "Byte compile FILENAME."
  (let* ((filename (expand-file-name filename))
         (result (byte-compile-file filename)) (compiled (eq result t)))
    (unless byte-compile-verbose
      (if compiled (message "Compiling %s..." filename)
        (message "Skipping %s..." filename)))
    compiled))

(eask-start
  (eask-pkg-init)
  (if-let ((files (or (eask-args) (eask-package-el-files))))
      (let (compiled)
        (eask-with-verbosity 'log
          (dolist (filename files)
            (when (eask--byte-compile-file filename) (push filename compiled))))
        (eask-info "(Total of %s file%s compiled)" (length compiled)
                   (eask--sinr compiled "" "s")))
    (eask-info "(No files have been compiled)")
    (eask--help-compile)))

;;; compile.el ends here

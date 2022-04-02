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
         (result))
    (eask-with-progress
      (format "Compiling %s..." filename)
      (eask-with-verbosity 'debug
        (setq result (byte-compile-file filename)
              result (eq result t)))
      (if result "done ✓" "skipped ✗"))
    result))

(defun eask--compile-files (files)
  "Compile sequence of files."
  (let* ((compiled (cl-remove-if-not #'eask--byte-compile-file files))
         (compiled (length compiled))
         (skipped (- (length files) compiled)))
    (eask-info "(Total of %s file%s compiled, %s skipped)" compiled
               (eask--sinr compiled "" "s")
               skipped)))

(eask-start
  (eask-pkg-init)
  (if-let ((files (or (eask-args) (eask-package-el-files))))
      (eask--compile-files files)
    (eask-info "(No files have been compiled)")
    (eask--help-compile)))

;;; compile.el ends here

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

(load-file (expand-file-name
            "_prepare.el"
            (file-name-directory (nth 1 (member "-scriptload" command-line-args)))))

(defun eask--byte-compile-file (filename)
  "Byte compile FILENAME with display messages."
  (let* ((result (byte-compile-file filename)) (compiled (eq result t)))
    (unless byte-compile-verbose
      (if compiled (message "Compiling %s..." filename)
        (message "Failed to compile %s..." filename)))
    compiled))

(eask-start
  (eask-pkg-init)
  (let ((files (or (eask-args) (eask-package-el-files))) compiled)
    (dolist (filename files)
      (add-to-list 'load-path (file-name-directory filename))
      (when (eask--byte-compile-file filename)
        (push filename compiled)))
    (message "\n Total of %s files compiled" (length compiled))))

;;; compile.el ends here

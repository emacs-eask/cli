;;; compile.el --- Byte compile all Emacs Lisp files in the package  -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; Byte compile all Emacs Lisp files in the package
;;
;;   $ eask compile
;;

;;; Code:

(load-file (expand-file-name
            "_prepare.el"
            (file-name-directory (nth 1 (member "-scriptload" command-line-args)))))

(defun eask--byte-compile-file (filename)
  "Byte compile FILENAME with display messages."
  (let ((byte-compile-verbose t)) (byte-compile-file filename)))

(eask-start
  (eask-pkg-init)
  (dolist (filename (eask-package-files))
    (add-to-list 'load-path (file-name-directory filename))
    (eask--byte-compile-file filename))
  (eask--byte-compile-file eask-package-file))

;;; compile.el ends here

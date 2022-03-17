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
  (when (member (file-name-extension filename) '("el"))
    (byte-compile-file filename)
    (unless byte-compile-verbose (message "Compiling %s..." filename))))

(eask-start
  (eask-pkg-init)
  (dolist (filename (eask-package-files))
    (add-to-list 'load-path (file-name-directory filename))
    (eask--byte-compile-file filename)))

;;; compile.el ends here

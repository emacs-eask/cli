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
  (byte-compile-file filename)
  (unless byte-compile-verbose (message "Compiling %s..." filename)))

(eask-start
  (eask-package-install 'package-build)
  (eask-pkg-init)
  (dolist (filename (or (eask-args) (eask-package-el-files)))
    (add-to-list 'load-path (file-name-directory filename))
    (eask--byte-compile-file filename)))

;;; compile.el ends here

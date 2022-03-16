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

(eask-start
  ;; TODO: ..
  (message ">> %s" eask-package-file)
  (message ">> %s" package-archives)
  (byte-compile-file eask-package-file)
  (dolist (pattern eask-files)
    (message "%s" (directory-files default-directory t pattern))
    )
  )

;;; compile.el ends here

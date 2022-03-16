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

(defun eask--f-entries (path pattern)
  "Return entries from PATH with PATTERN."
  (when (file-directory-p path)
    (cl-remove-if-not
     (lambda (file) (string-match-p pattern file))
     (directory-files-recursively path "^\\([^.]\\|\\.[^.]\\|\\.\\..\\)"))))

(defun eask--byte-compile-file (filename)
  "Byte compile FILENAME with display messages."
  (message "Compiling file... %s" filename)
  (byte-compile-file filename))

(eask-start
  (package-initialize)
  (package-refresh-contents)
  (dolist (pattern eask-files)
    (dolist (filename (eask--f-entries default-directory pattern))
      ;; XXX Optionally, we can just ignore package-file?
      (unless (equal filename eask-package-file)
        (add-to-list 'load-path (file-name-directory filename))
        (eask--byte-compile-file filename))))
  (eask--byte-compile-file eask-package-file))

;;; compile.el ends here

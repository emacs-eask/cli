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

(defvar eask--compiled-files nil
  "Cache for compiled files.")

(defun eask--filter-exclude-dirs (item)
  "Filter out ITEM from default ignore paths."
  (not (cl-some (lambda (elm) (string-match-p elm item)) eask-path-ignores)))

(defun eask--f-entries (path pattern)
  "Return entries from PATH with PATTERN."
  (when (file-directory-p path)
    (cl-remove-if-not
     (lambda (file) (string-match-p pattern file))
     (directory-files-recursively path "^\\([^.]\\|\\.[^.]\\|\\.\\..\\)" nil
                                  #'eask--filter-exclude-dirs))))

(defun eask--byte-compile-file (filename)
  "Byte compile FILENAME with display messages."
  (unless (member filename eask--compiled-files)
    (let ((byte-compile-verbose t)) (byte-compile-file filename))
    (push filename eask--compiled-files)))

(eask-start
  (package-initialize)
  (package-refresh-contents)
  (dolist (pattern eask-files)
    (dolist (filename (eask--f-entries default-directory pattern))
      (add-to-list 'load-path (file-name-directory filename))
      (eask--byte-compile-file filename)))
  (eask--byte-compile-file eask-package-file))

;;; compile.el ends here

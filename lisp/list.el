;;; list.el --- List all installed packages  -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; Command use to list out installed Emacs packages,
;;
;;   $ eask list
;;
;;
;;  Action options:
;;
;;    [-g]       list all packages default to `~/.emacs.d/'
;;    [--depth]  dependency level to print
;;

;;; Code:

(load-file (expand-file-name
            "_prepare.el"
            (file-name-directory (nth 1 (member "-scriptload" command-line-args)))))

(defvar eask-list-package-name-width nil
  "Width spaces for the package name.")

(defun eask--format (depth &optional rest)
  "Format string to align starting from the version number."
  (concat (spaces-string (* depth 2))  ; indent for depth
          " [+] %-" (number-to-string (- eask-list-package-name-width (* depth 2)))
          "s " rest))

(defun eask-print-pkg (name depth max-depth pkg-alist)
  "Print NAME package information."
  (when-let*
      ((pkg (assq name pkg-alist))
       (desc (cadr pkg))
       (name (package-desc-name desc))
       (version (package-desc-version desc))
       (version (package-version-join version))
       (summary (package-desc-summary desc)))
    (if (= depth 0)
        (message (eask--format depth "%-14s %-80s") name version summary)
      (message (eask--format depth) name))
    (when-let ((reqs (package-desc-reqs desc))
               (_ (< depth max-depth)))
      (dolist (req reqs)
        (eask-print-pkg (car req) (1+ depth) max-depth pkg-alist)))))

(defun eask-seq-max-str (sequence)
  "Return max length in SEQUENCE."
  (let ((result 0))
    (mapc (lambda (elm) (setq result (max result (length (format "%s" elm))))) sequence)
    result))

(defun eask--list (list pkg-alist &optional depth)
  "List packages."
  (let ((eask-list-package-name-width (+ (eask-seq-max-str list) 1)))
    (dolist (name list)
      (eask-print-pkg name 0 (or depth (eask-depth) 999) pkg-alist))))

(eask-start
  (eask-pkg-init)
  (eask--list package-activated-list package-alist)
  (message "\n Total of %s packages installed" (length package-activated-list)))

;;; list.el ends here

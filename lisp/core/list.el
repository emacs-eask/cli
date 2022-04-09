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
;;    [--depth]  dependency level to print
;;

;;; Code:

(load (expand-file-name
       "../_prepare.el"
       (file-name-directory (nth 1 (member "-scriptload" command-line-args))))
      nil t)

(defvar eask-list-package-name-width nil
  "Width spaces for the package name.")

(defun eask--align (depth &optional rest)
  "Format string to align starting from the version number."
  (concat (spaces-string (* depth 2))  ; indent for depth
          " " (if (= depth 0) "[+]" "[+]") " %-"
          (number-to-string (- eask-list-package-name-width (* depth 2)))
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
        (message (eask--align depth "%-14s %-80s") name version summary)
      (message (eask--align depth) name))
    (when-let ((reqs (package-desc-reqs desc))
               ((< depth max-depth)))
      (dolist (req reqs)
        (eask-print-pkg (car req) (1+ depth) max-depth pkg-alist)))))

(defun eask--list (list pkg-alist &optional depth)
  "List packages."
  (let ((eask-list-package-name-width (+ (eask-seq-str-max list) 1)))
    (dolist (name list)
      (eask-print-pkg name 0 (or depth (eask-depth) 999) pkg-alist))))

(eask-start
  (eask-pkg-init)
  (eask--list package-activated-list package-alist)
  (eask-info "(Total of %s package%s installed)"
             (length package-activated-list)
             (eask--sinr package-activated-list "" "s")))

;;; list.el ends here

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

(defvar eask--list-pkg-name-offset nil)
(defvar eask--list-pkg-version-offset nil)
(defvar eask--list-pkg-archive-offset nil)

(defun eask--format-s (offset)
  "Format OFFSET."
  (concat " %-" (number-to-string offset) "s "))

(defun eask--align (depth &optional rest)
  "Format string to align starting from the version number."
  (let ((prefix (if (= depth 0) "[+]" "[+]")))
    (concat (spaces-string (* depth 2))  ; indent for depth
            " " prefix
            (eask--format-s (- eask--list-pkg-name-offset (* depth 2)))
            (eask--format-s eask--list-pkg-version-offset)
            (eask--format-s eask--list-pkg-archive-offset)
            rest)))

(defun eask-print-pkg (name depth max-depth pkg-alist)
  "Print NAME package information."
  (when-let*
      ((pkg (assq name pkg-alist))
       (desc (cadr pkg))
       (name (package-desc-name desc))
       (version (package-desc-version desc))
       (version (package-version-join version))
       (archive (or (package-desc-archive desc) ""))
       (summary (package-desc-summary desc)))
    (if (= depth 0)
        (eask-msg (eask--align depth " %-80s") name version archive summary)
      (eask-msg (eask--align depth) name "" "" ""))
    (when-let ((reqs (package-desc-reqs desc))
               ((< depth max-depth)))
      (dolist (req reqs)
        (eask-print-pkg (car req) (1+ depth) max-depth pkg-alist)))))

(defun eask--version-list (pkg-alist)
  "Return list of versions."
  (mapcar (lambda (elm)
            (package-version-join (package-desc-version (cadr elm))))
          pkg-alist))

(defun eask--archive-list (pkg-alist)
  "Return list of archives."
  (mapcar (lambda (elm)
            (or (package-desc-archive (cadr elm)) ""))
          pkg-alist))

(defun eask--list (list pkg-alist &optional depth)
  "List packages."
  (let* ((eask--list-pkg-name-offset (eask-seq-str-max list))
         (version-list (eask--version-list pkg-alist))
         (eask--list-pkg-version-offset (eask-seq-str-max version-list))
         (archive-list (eask--archive-list pkg-alist))
         (eask--list-pkg-archive-offset (eask-seq-str-max archive-list)))
    (dolist (name list)
      (eask-print-pkg name 0 (or depth (eask-depth) 999) pkg-alist))))

(eask-start
  (eask-defvc< 27 (eask-pkg-init))  ; XXX: remove this after we drop 26.x
  (eask--list package-activated-list package-alist)
  (eask-info "(Total of %s package%s installed)"
             (length package-activated-list)
             (eask--sinr package-activated-list "" "s")))

;;; list.el ends here

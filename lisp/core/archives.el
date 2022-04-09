;;; archives.el --- List out all package archives  -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; Command use to list out all package archives,
;;
;;   $ eask archives
;;

;;; Code:

(load (expand-file-name
       "../_prepare.el"
       (file-name-directory (nth 1 (member "-scriptload" command-line-args))))
      nil t)

(defvar eask--length-name)
(defvar eask--length-url)
(defvar eask--length-priority)

(defun eask--print-archive (archive)
  "Print the archive."
  (let* ((name (car archive))
         (url (cdr archive))
         (priority (assoc name package-archive-priorities))
         (priority (cdr priority)))
    (message (concat "  %-" eask--length-name "s  %-" eask--length-url "s  %-" eask--length-priority "s")
             name url (or priority 0))))

(eask-start
  (if package-archives
      (progn
        (let* ((names (mapcar #'car package-archives))
               (eask--length-name (format "%s" (eask-seq-str-max names)))
               (urls (mapcar #'cdr package-archives))
               (eask--length-url (format "%s" (eask-seq-str-max urls)))
               (priorities (mapcar #'cdr package-archive-priorities))
               (eask--length-priority (format "%s" (eask-seq-str-max priorities))))
          (mapc #'eask--print-archive package-archives))
        (eask-info "(Total of %s archives)" (length package-archives)))
    (eask-info "(No archive has been selected)")))

;;; archives.el ends here

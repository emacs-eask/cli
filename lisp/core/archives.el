;;; core/archives.el --- List out all package archives  -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; Command use to list out all package archives,
;;
;;   $ eask archives
;;

;;; Code:

(let ((dir (file-name-directory (nth 1 (member "-scriptload" command-line-args)))))
  (load (expand-file-name "_prepare.el"
                          (locate-dominating-file dir "_prepare.el"))
        nil t))

(defvar eask-archive--length-name)
(defvar eask-archive--length-url)
(defvar eask-archive--length-priority)

(defun eask-archive--print (archive)
  "Print the ARCHIVE."
  (let* ((name (car archive))
         (url (cdr archive))
         (priority (assoc name package-archive-priorities))
         (priority (cdr priority)))
    (eask-println
     (concat "  %-" eask-archive--length-name "s  %-" eask-archive--length-url
             "s  %-" eask-archive--length-priority "s")
     name (eask-2url url) (or priority 0))))

(defun eask-archive--print-alist (alist)
  "Print the archvie ALIST."
  (let* ((names (mapcar #'car alist))
         (eask-archive--length-name (eask-2str (eask-seq-str-max names)))
         (urls (mapcar #'cdr alist))
         (eask-archive--length-url (eask-2str (eask-seq-str-max urls)))
         (priorities (mapcar #'cdr package-archive-priorities))
         (eask-archive--length-priority (eask-2str (eask-seq-str-max priorities))))
    (mapc #'eask-archive--print alist)))

(eask-start
  (cond
   ((eask-all-p)
    (eask-info "Available archives:")
    (eask-msg "")
    (eask-archive--print-alist eask-source-mapping)
    (eask-info "(Total of %s archive%s available)" (length eask-source-mapping)
               (eask--sinr eask-source-mapping "" "s")))
   (package-archives
    (eask-info "In used archives:")
    (eask-msg "")
    (eask-archive--print-alist package-archives)
    (eask-info "(Total of %s archive%s listed)" (length package-archives)
               (eask--sinr package-archives "" "s")))
   (t
    (eask-info "(No archive has been selected)"))))

;;; core/archives.el ends here

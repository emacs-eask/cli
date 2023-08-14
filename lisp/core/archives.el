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

(defvar eask--length-name)
(defvar eask--length-url)
(defvar eask--length-priority)

(defun eask--print-archive (archive)
  "Print the ARCHIVE."
  (let* ((name (car archive))
         (url (cdr archive))
         (priority (assoc name package-archive-priorities))
         (priority (cdr priority)))
    (message (concat "  %-" eask--length-name "s  %-" eask--length-url "s  %-" eask--length-priority "s")
             name url (or priority 0))))

(defun eask--print-archive-alist (alist)
  "Print the archvie ALIST."
  (let* ((names (mapcar #'car alist))
         (eask--length-name (eask-2str (eask-seq-str-max names)))
         (urls (mapcar #'cdr alist))
         (eask--length-url (eask-2str (eask-seq-str-max urls)))
         (priorities (mapcar #'cdr package-archive-priorities))
         (eask--length-priority (eask-2str (eask-seq-str-max priorities))))
    (mapc #'eask--print-archive alist)))

(eask-start
  (cond
   ((eask-all-p)
    (eask-info "Available archives:")
    (eask-msg "")
    (eask--print-archive-alist eask-source-mapping)
    (eask-msg "")
    (eask-info "(Total of %s archive%s available)" (length eask-source-mapping)
               (eask--sinr eask-source-mapping "" "s")))
   (package-archives
    (eask-info "In used archives:")
    (eask-msg "")
    (eask--print-archive-alist package-archives)
    (eask-msg "")
    (eask-info "(Total of %s archive%s listed)" (length package-archives)
               (eask--sinr package-archives "" "s")))
   (t
    (eask-info "(No archive has been selected)"))))

;;; core/archives.el ends here

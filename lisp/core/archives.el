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

(defun eask--print-archive (archive)
  "Print the archive."
  (let* ((extract (mapcar #'car package-archives))
         (len (eask-seq-str-max extract))
         (len (format "%s" len)))
    (message (concat "  [+] %-" len "s  %s")
             (car archive) (cdr archive))))

(eask-start
  (if package-archives
      (progn
        (mapc #'eask--print-archive package-archives)
        (eask-info "(Total of %s archives)" (length package-archives)))
    (eask-info "(No archive has been selected)")))

;;; archives.el ends here

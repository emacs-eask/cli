;;; keywords.el --- List all available keywords  -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; List available keywords that can be used in the header section,
;;
;;   $ eask keywords
;;

;;; Code:

(load (expand-file-name
       "../_prepare.el"
       (file-name-directory (nth 1 (member "-scriptload" command-line-args))))
      nil t)

(require 'finder)

(eask-start
  (let* ((keywords (mapcar #'car finder-known-keywords))
         (offset (eask-seq-str-max keywords))
         (fmt (concat "  %-" (eask-2str offset) "s  %s")))
    (dolist (keyword keywords)
      (eask-msg fmt keyword (cdr (assq keyword finder-known-keywords))))
    (eask-msg "")
    (eask-info "(Total of %s keywords listed)" (length keywords))))

;;; keywords.el ends here

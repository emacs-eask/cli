;;; search.el --- Search packages  -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; Command use to search Emacs packges,
;;
;;   $ eask search [queries..]
;;
;;
;;  Initialization options:
;;
;;    [queries..]     query to search packages
;;

;;; Code:

(load (expand-file-name
       "../_prepare.el"
       (file-name-directory (nth 1 (member "-scriptload" command-line-args))))
      nil t)

(eask-load "core/list")

(defun eask--search-packages (query)
  "Filter available packages with QUERY."
  (let ((result))
    (dolist (package (mapcar #'car package-archive-contents))
      (when (string-match-p query (format "%s" package))
        (push package result)))
    result))

(eask-start
  (eask-pkg-init)
  (if-let ((queries (eask-args)))
      (let ((result))
        (dolist (query queries)
          (setq result (append result (eask--search-packages query))))
        (delete-dups result)
        (eask--list result package-archive-contents)
        (eask-info "(Search result of %s package%s)" (length result)
                   (eask--sinr result "" "s")))
    (eask-info "(No search operation; missing queries specification)")
    (eask-help 'search)))

;;; search.el ends here

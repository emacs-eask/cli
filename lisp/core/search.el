;;; core/search.el --- Search packages  -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; Command use to search Emacs packges,
;;
;;   $ eask search [queries..]
;;
;;
;;  Positionals:
;;
;;    [queries..]     query to search packages
;;

;;; Code:

(let ((dir (file-name-directory (nth 1 (member "-scriptload" command-line-args)))))
  (load (expand-file-name "_prepare.el"
                          (locate-dominating-file dir "_prepare.el"))
        nil t))

(eask-load "core/list")

(defun eask-search--packages (query)
  "Filter available packages with QUERY."
  (let ((result))
    (dolist (package (mapcar #'car package-archive-contents))
      (when (string-match-p query (eask-2str package))
        (push package result)))
    result))

(eask-start
  (eask-pkg-init)
  (if-let ((queries (eask-args)))
      (let ((result))
        (dolist (query queries)
          (setq result (append result (eask-search--packages query))))
        (delete-dups result)
        (eask-list result package-archive-contents)
        (eask-msg "")
        (eask-info "(Search result of %s package%s)" (length result)
                   (eask--sinr result "" "s")))
    (eask-msg "")
    (eask-info "(No search operation; missing queries specification)")
    (eask-help "core/search")))

;;; core/search.el ends here

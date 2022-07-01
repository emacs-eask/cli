;;; keywords.el --- Lint the package's keywords header  -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; Command use to check keywords header inside the package,
;;
;;   $ eask lint keywords
;;

;;; Code:

(load (expand-file-name
       "../_prepare.el"
       (file-name-directory (nth 1 (member "-scriptload" command-line-args))))
      nil t)

(require 'finder)

(defun eask--defined-keywords (keywords)
  "Return t if KEYWORDS are defined correctly."
  (let ((available-keywords (mapcar #'car finder-known-keywords))
        (result))
    (dolist (keyword keywords)
      (when (member keyword available-keywords)
        (setq result t)))
    result))

(eask-start
  (let ((keywords (eask-package-desc-keywords))
        (file (eask-root-del eask-package-file)))
    (cond
     ((not eask-package-file)
      (eask-report "Can't lint keywords without the package-file specified")
      (eask-help "lint/keywords-file"))
     ((not keywords)
      (eask-report "Keywords header seems to be missing in the `%s' file" file)
      (eask-help "lint/keywords-header"))
     (t
      (eask-msg "")
      (eask-msg "`%s` with keywords-lint" (ansi-green file))
      (if (eask--defined-keywords keywords)
          (eask-info "(No issues found.)")
        (eask-report "Missing a standard keyword, consider adding one to the Keywords header!")
        (eask-help "lint/keywords"))))))

;;; keywords.el ends here

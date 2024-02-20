;;; lint/keywords.el --- Lint the package's keywords header  -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; Command use to check keywords header inside the package,
;;
;;   $ eask lint keywords
;;

;;; Code:

(let ((dir (file-name-directory (nth 1 (member "-scriptload" command-line-args)))))
  (load (expand-file-name "_prepare.el"
                          (locate-dominating-file dir "_prepare.el"))
        nil t))

;;
;;; Flags

(advice-add #'eask-allow-error-p :override #'always)

;;
;;; Core

(require 'finder)

(defun eask-lint-keywords--defined (keywords)
  "Return t if KEYWORDS are defined correctly."
  (let ((available-keywords (mapcar #'car finder-known-keywords))
        (result))
    (dolist (keyword keywords)
      (when (memq (intern keyword) available-keywords)
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
      (eask-lint-first-newline)
      (eask-msg "`%s` with keywords-lint" (ansi-green file))
      (if (eask-lint-keywords--defined keywords)
          (progn
            (eask-msg "")
            (eask-info "(No issues found.)"))
        (eask-report "Missing a standard keyword, consider adding one to the Keywords header!")
        (eask-help "lint/keywords"))))))

;;; lint/keywords.el ends here

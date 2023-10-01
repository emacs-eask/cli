;;; generate/recipe.el --- Generate recipe file  -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; Command use generate recipe file,
;;
;;   $ eask generate recipe [destination]
;;
;;
;;  Positional options:
;;
;;    [destination]      destination path/folder
;;

;;; Code:

(let ((dir (file-name-directory (nth 1 (member "-scriptload" command-line-args)))))
  (load (expand-file-name "_prepare.el"
                          (locate-dominating-file dir "_prepare.el"))
        nil t))

(eask-load "core/recipe")

;;
;;; Core

(eask-start
  (if-let ((recipe (eask--recipe-string))
           (name (eask-guess-package-name)))
      (let* ((eask-recipe-path (or (eask-args 0) eask-recipe-path))
             (eask-recipe-path (expand-file-name eask-recipe-path))
             (recipe-file (expand-file-name name eask-recipe-path))
             (recipe-string (pp-to-string recipe)))
        (when (or (eask-yes-p)
                  (yes-or-no-p (format "%s\nIs this OK? " recipe-string)))
          ;; XXX: Just to fake the user input!
          (when (eask-yes-p)
            (eask-msg (format "%s\nIs this OK? (yes or no) yes" recipe-string)))
          (ignore-errors (make-directory eask-recipe-path t))
          (with-current-buffer (find-file recipe-file)
            (erase-buffer)
            (insert recipe-string)
            (save-buffer))
          (eask-info "(Generated in %s)" recipe-file)))
    (eask-msg "")
    (eask-info "(Repository URL is required to form a recipe)")
    (eask-help "core/recipe")))

;;; generate/recipe.el ends here

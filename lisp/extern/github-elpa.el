;;; github-elpa.el --- External module `github-elpa'  -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(unless eask-depends-on-recipe-p
  (defvar github-elpa-working-dir (expand-file-name "./temp-elpa/.working/" user-emacs-directory))
  (defvar github-elpa-archive-dir (expand-file-name "./temp-elpa/packages/" user-emacs-directory))
  (defvar github-elpa-recipes-dir (expand-file-name "./temp-elpa/recipes/" user-emacs-directory))

  (ignore-errors (make-directory   github-elpa-working-dir t))
  (ignore-errors (make-directory   github-elpa-archive-dir t))

  (ignore-errors (delete-directory github-elpa-recipes-dir t))
  (ignore-errors (make-directory   github-elpa-recipes-dir t)))

(defun github-elpa-build ()
  "Github elpa build."
  (eask-load "extern/package-build")  ; override
  (let ((package-build-working-dir github-elpa-working-dir)
        (package-build-archive-dir github-elpa-archive-dir)
        (package-build-recipes-dir github-elpa-recipes-dir))
    ;;(github-elpa--git-check-repo)
    ;;(github-elpa--git-check-workdir-clean)
    (make-directory package-build-archive-dir t)
    ;; Currently no way to detect build failure...
    (dolist (recipe (directory-files package-build-recipes-dir nil "^[^.]"))
      (message "")
      (message "")
      (message ":: temp-elpa: packaging recipe %s" recipe)
      (package-build-archive recipe))
    (package-build-cleanup)))

;;; github-elpa.el ends here

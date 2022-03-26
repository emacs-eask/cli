;;; package.el --- Build a package artefact  -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; Build a package artefact, and put it into the given destination
;;
;;   $ eask package [dest]
;;
;;
;;  Positional options:
;;
;;    [dest]     destination path/folder
;;

;;; Code:

(load-file (expand-file-name
            "_prepare.el"
            (file-name-directory (nth 1 (member "-scriptload" command-line-args)))))

(defcustom eask-dist-path "dist"
  "Name of default target directory for building packages."
  :type 'string
  :group 'eask)

(defun eask-package-dir-recipe ()
  "Form a directory recipe."
  (eask-load "./extern/package-recipe")
  (let ((name (eask-guess-package-name))
        (patterns (or eask-files package-build-default-files-spec))
        (path default-directory))
    (package-directory-recipe name :name name :files patterns :dir path)))

(eask-start
  (let ((dest (or (eask-argv 0) eask-dist-path)))
    (ignore-errors (make-directory (expand-file-name dest) t))

    (eask-package-install 'package-build)
    (eask-load "./extern/package-build")  ; override

    (let* ((version (eask-package-get :version))
           (rcp (eask-package-dir-recipe))
           (package-build-working-dir default-directory)
           (package-build-archive-dir (expand-file-name dest)))
      (package-build--package rcp version))
    (message "\n Done.")))

;;; package.el ends here

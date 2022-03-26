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

(eask-start
  (let ((dest (or (eask-argv 0) eask-dist-path)))
    (ignore-errors (make-directory (expand-file-name dest) t))

    (eask-package-install 'package-build)
    (require 'package-recipe)
    (eask-load "./extern/package-build")  ; override
    (eask-load "./extern/package-recipe")

    (let* ((name (eask-guess-package-name))
           (patterns (or eask-files package-build-default-files-spec))
           (path default-directory)
           (version (eask-package-get :version))
           (rcp (package-directory-recipe name :name name :files patterns :dir path))
           (package-build-working-dir path)
           (package-build-archive-dir (expand-file-name dest)))
      (package-build--package rcp version))
    (message "\n Done.")))

;;; package.el ends here

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

(defun eask-packaged-name ()
  "Find a possible packaged name."
  (let ((name (eask-guess-package-name))
        (version (eask-package-get :version)))
    (concat name "-" version)))

(defun eask--packaged-file (ext)
  "Find a possible packaged file."
  (let* ((dist (expand-file-name eask-dist-path))
         (file (expand-file-name (concat (eask-packaged-name) "." ext) dist)))
    (and (file-exists-p file) file)))

(defun eask-packaged-file ()
  "Return generated pacakge artefact."
  (or (eask--packaged-file "tar") (eask--packaged-file "el")))

(eask-start
  (let ((eask-dist-path (or (eask-argv 0) eask-dist-path))
        (packaged))
    (ignore-errors (make-directory (expand-file-name eask-dist-path) t))

    (eask-pkg-init)
    (eask-package-install 'package-build)
    (eask-load "./extern/package-build")  ; override

    (let* ((version (eask-package-get :version))
           (rcp (eask-package-dir-recipe))
           (package-build-working-dir default-directory)
           (package-build-archive-dir (expand-file-name eask-dist-path)))
      (package-build--package rcp version))

    (setq packaged (eask-packaged-file))

    (when (and eask-is-windows (string= (file-name-extension packaged) "el"))
      (with-current-buffer (find-file packaged)
        (set-buffer-file-coding-system 'utf-8-unix)
        (save-buffer)))

    (eask-info "Done packaging %s..." packaged)))

;;; package.el ends here

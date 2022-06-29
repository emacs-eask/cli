;;; package.el --- Build a package artifact  -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; Build a package artifact, and put it into the given destination
;;
;;   $ eask package [destination]
;;
;;
;;  Positional options:
;;
;;    [destination]      destination path/folder
;;

;;; Code:

(load (expand-file-name
       "../_prepare.el"
       (file-name-directory (nth 1 (member "-scriptload" command-line-args))))
      nil t)

(defun eask-package-dir-recipe ()
  "Form a directory recipe."
  (eask-load "extern/package-recipe")
  (let* ((name (eask-guess-package-name))
         (files (when eask-files (append eask-files (list eask-package-file))))
         (patterns (or files package-build-default-files-spec))
         (path default-directory))
    (package-directory-recipe name :name name :files patterns :dir path)))

(defun eask-packaged-name ()
  "Find a possible packaged name."
  (let ((name (eask-guess-package-name))
        (version (eask-package-version)))
    (concat name "-" version)))

(defun eask--packaged-file (ext)
  "Find a possible packaged file."
  (let* ((dist eask-dist-path)
         (file (expand-file-name (concat (eask-packaged-name) "." ext) dist)))
    (and (file-exists-p file) file)))

(defun eask-packaged-file ()
  "Return generated package artifact; it could be a tar or el."
  (if (eask-package-multi-p) (eask--packaged-file "tar")
    (eask--packaged-file "el")))

(eask-start
  (let ((eask-dist-path (or (eask-argv 0) eask-dist-path))
        (eask-dist-path (expand-file-name eask-dist-path))
        (packaged))
    (ignore-errors (make-directory eask-dist-path t))

    (eask-defvc< 27 (eask-pkg-init))  ; XXX: remove this after we drop 26.x
    (eask-with-archives "melpa"
      (eask-package-install 'package-build))
    (eask-load "extern/package-build")  ; override

    (let* ((version (eask-package-version))
           (rcp (eask-package-dir-recipe))
           (package-build-working-dir default-directory)
           (package-build-archive-dir eask-dist-path))
      (eask-with-progress
        (format "Building artifact %s (%s)... " (eask-package-name) version)
        (package-build--package rcp version)
        "done ✓"))

    (setq packaged (eask-packaged-file))

    (when (and eask-is-windows (string= (file-name-extension packaged) "el"))
      (with-current-buffer (find-file packaged)
        (set-buffer-file-coding-system 'utf-8-unix)
        (save-buffer)))

    (eask-info "(Built in %s)" packaged)))

;;; package.el ends here

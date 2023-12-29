;;; core/package.el --- Build a package artifact  -*- lexical-binding: t; -*-

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

(let ((dir (file-name-directory (nth 1 (member "-scriptload" command-line-args)))))
  (load (expand-file-name "_prepare.el"
                          (locate-dominating-file dir "_prepare.el"))
        nil t))

;;
;;; Externals

(declare-function package-directory-recipe "ext:package-recipe.el")

;;
;;; Core

(defun eask-package-dir--patterns ()
  "Return patterns for directory recipe."
  (if eask-files
      (if (member eask-package-file (eask-expand-file-specs (eask-files-spec)))
          ;; Else we return default
          eask-files
        ;; If files DSL doesn't contain package main file, we added manually!
        ;;
        ;; This would avoid error, single file doesn't match package name.
        (append eask-files (list eask-package-file)))
    package-build-default-files-spec))

(defun eask-package-dir-recipe (version)
  "Form a directory recipe.

Argument VERSION is a string represent the version number of this package."
  (eask-load "extern/package-recipe")
  (let* ((name (eask-guess-package-name))
         (patterns (eask-package-dir--patterns))
         (path default-directory)
         (rcp (package-directory-recipe name :name name :files patterns :dir path)))
    (setf (slot-value rcp 'version) version)
    (setf (slot-value rcp 'time) (eask-current-time))
    rcp))

(defun eask-packaged-name ()
  "Find a possible packaged name."
  (let ((name (eask-guess-package-name))
        (version (eask-package-version)))
    (concat name "-" version)))

(defun eask--packaged-file (ext)
  "Find a possible packaged file with extension (EXT)."
  (expand-file-name (concat (eask-packaged-name) "." ext) eask-dist-path))

(defun eask-packaged-file ()
  "Return generated package artifact; it could be a tar or el."
  (if (eask-package-multi-p) (eask--packaged-file "tar")
    (eask--packaged-file "el")))

(eask-start
  (let* ((eask-dist-path (or (eask-args 0) eask-dist-path))
         (eask-dist-path (expand-file-name eask-dist-path))
         (packaged))
    (ignore-errors (make-directory eask-dist-path t))

    (eask-defvc< 27 (eask-pkg-init))  ; XXX: remove this after we drop 26.x
    (eask-with-archives "melpa"
      (eask-package-install 'package-build))
    (eask-load "extern/package-build")  ; override

    (let* ((version (eask-package-version))
           (rcp (eask-package-dir-recipe version))
           (package-build-working-dir default-directory)
           (package-build-archive-dir eask-dist-path))
      (eask-msg "")
      (eask-with-progress
        (format "Building artifact %s (%s)... " (eask-package-name) version)
        (package-build--package rcp)
        "done ✓"))

    (setq packaged (eask-packaged-file)
          packaged (when (file-exists-p packaged) packaged))

    (when (and eask-is-windows (eask-package-single-p))
      (with-current-buffer (find-file packaged)
        (set-buffer-file-coding-system 'utf-8-unix)
        (save-buffer)))

    (eask-msg "")
    (eask-info "(Built in %s)" packaged)))

;;; core/package.el ends here

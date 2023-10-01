;;; clean/dist.el --- Delete dist subdirectory  -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; Command that delete dist subdirectory,
;;
;;   $ eask clean dist [destination]
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

(eask-load "core/package")  ; load dist path

(defun eask--clean-dist (path)
  "Clean up dist PATH."
  (let* ((name (eask-guess-package-name))
         (version (eask-package-version))
         (readme (expand-file-name (format "%s-readme.txt" name) path))
         (entry (expand-file-name (format "%s-%s.entry" name version) path))
         (packaged (eask-packaged-file))
         (deleted 0)
         (delete-dir))
    (when (eask-delete-file readme)   (cl-incf deleted))
    (when (eask-delete-file entry)    (cl-incf deleted))
    (when (eask-delete-file packaged) (cl-incf deleted))
    (when (and (not (zerop deleted)) (eask-directory-empty-p path))
      (eask-with-progress
        (format "The dist folder %s seems to be empty, delete it as well... " path)
        (ignore-errors (delete-directory path))
        "done ✓")
      (setq delete-dir t))
    (eask-msg "")
    (eask-info "(Total of %s file%s and %s directory deleted, %s skipped)" deleted
               (eask--sinr deleted "" "s")
               (if delete-dir "1" "0")
               (- 3 deleted))))

(eask-start
  (let* ((eask-dist-path (or (eask-args 0) eask-dist-path))
         (eask-dist-path (expand-file-name eask-dist-path)))
    (if (file-directory-p eask-dist-path)
        (eask--clean-dist eask-dist-path)
      (eask-info "(No dist folder needs to be cleaned)" eask-dist-path)
      (setq eask-no-cleaning-operation-p t))))

;;; clean/dist.el ends here

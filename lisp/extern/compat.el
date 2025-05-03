;;; extern/compat.el --- Compatible API  -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

;; XXX: Support after Emacs 27.1
(defun eask-directory-files-recursively ( dir regexp
                                          &optional include-directories predicate
                                          follow-symlinks)
  "..."
  (let* ((result nil)
         (files nil)
         (dir (directory-file-name dir))
         ;; When DIR is "/", remote file names like "/method:" could
         ;; also be offered.  We shall suppress them.
         (tramp-mode (and tramp-mode (file-remote-p (expand-file-name dir)))))
    (dolist (file (sort (file-name-all-completions "" dir)
                        'string<))
      (unless (member file '("./" "../"))
        (if (directory-name-p file)
            (let* ((leaf (substring file 0 (1- (length file))))
                   (full-file (concat dir "/" leaf)))
              ;; Don't follow symlinks to other directories.
              (when (and (or (not (file-symlink-p full-file))
                             (and (file-symlink-p full-file)
                                  follow-symlinks))
                         ;; Allow filtering subdirectories.
                         (or (eq predicate nil)
                             (eq predicate t)
                             (funcall predicate full-file)))
                (let ((sub-files
                       (if (eq predicate t)
                           (ignore-error file-error
                             (eask-directory-files-recursively
                              full-file regexp include-directories
                              predicate follow-symlinks))
                         (eask-directory-files-recursively
                          full-file regexp include-directories
                          predicate follow-symlinks))))
                  (setq result (nconc result sub-files))))
              (when (and include-directories
                         (string-match regexp leaf))
                (setq result (nconc result (list full-file)))))
          (when (string-match regexp file)
            (push (concat dir "/" file) files)))))
    (nconc result (nreverse files))))

;; XXX: Support after Emacs 28.1
(defun eask-always (&rest _arguments)
  "..."
  t)

;;; extern/compat.el ends here

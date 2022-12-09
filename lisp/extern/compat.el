;;; extern/compat.el --- Compatible API  -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(eask-defvc< 28
  (defun directory-empty-p (dir)
    "..."
    (and (file-directory-p dir)
         (null (directory-files dir nil directory-files-no-dot-files-regexp t 1)))))

;;; extern/compat.el ends here

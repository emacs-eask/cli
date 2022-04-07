;;; package.el --- External module `package'  -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; This module is used to compatible with older Emacs version.
;;

;;; Code:

;;
;;; Emacs 28.1
(eask-defun-fbound 'package--archives-initialize
  (defun package--archives-initialize ()
    "Make sure the list of installed and remote packages are initialized."
    (unless package--initialized
      (package-initialize t))
    (unless package-archive-contents
      (package-refresh-contents))))

;;; package.el ends here

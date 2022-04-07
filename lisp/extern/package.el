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

;;
;;; Emacs 27.1
(eask-defun-fbound 'package-activate-all
  (defun package-activate-all ()
    "Activate all installed packages.
The variable `package-load-list' controls which packages to load."
    (setq package--activated t)
    (let* ((elc (concat package-quickstart-file "c"))
           (qs (if (file-readable-p elc) elc
                 (if (file-readable-p package-quickstart-file)
                     package-quickstart-file))))
      (if qs
          ;; Skip load-source-file-function which would slow us down by a factor
          ;; 2 when loading the .el file (this assumes we were careful to
          ;; save this file so it doesn't need any decoding).
          (let ((load-source-file-function nil))
            (unless (boundp 'package-activated-list)
              (setq package-activated-list nil))
            (load qs nil 'nomessage))
        (require 'package)
        (package--activate-all)))))

;;; package.el ends here

;;; package.el --- External module `package'  -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; This module is used to compatible with older Emacs version.
;;

;;; Code:

(eask-defvc< 28
  (defvar package-quickstart-file
    (locate-user-emacs-file "package-quickstart.el"))

  (defun package--alist ()
    "Return `package-alist', after computing it if needed."
    (or package-alist
        (progn (package-load-all-descriptors)
               package-alist)))

  (defun package--activate-all ()
    (dolist (elt (package--alist))
      (condition-case err
          (package-activate (car elt))
        ;; Don't let failure of activation of a package arbitrarily stop
        ;; activation of further packages.
        (error (message "%s" (error-message-string err))))))

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

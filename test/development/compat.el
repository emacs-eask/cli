;;; compat.el --- Test Emacs compatibility  -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(defconst compat-functions
  '(url-file-exists-p
    package--alist
    package--activate-all
    package-activate-all)
  "List of function to check Emacs compatibility.")

(dolist (func compat-functions)
  (let ((exists-p))
    (eask-with-progress
      (format "Checked function %s... " func)
      (setq exists-p (fboundp func))
      (if exists-p "done ✓" "missing ✗"))
    (unless exists-p
      (eask-error "Missing function %s in Emacs version %s... " func emacs-version))))

(defconst compat-variables
  '(package-quickstart-file)
  "List of variables to check Emacs compatibility.")

(dolist (var compat-variables)
  (let ((exists-p))
    (eask-with-progress
      (format "Checked variable %s... " var)
      (setq exists-p (boundp var))
      (if exists-p "done ✓" "missing ✗"))
    (unless exists-p
      (eask-error "Missing variable %s in Emacs version %s... " var emacs-version))))

;;; compat.el ends here

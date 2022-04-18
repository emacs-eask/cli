;;; compat.el --- Test Emacs compatibility  -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; Test for Emacs' functions and variables existense.
;;
;; See the test result in
;; (https://github.com/emacs-eask/eask/actions/workflows/test-outdated_upgrade.yml).
;;

;;; Code:

;;
;;; Test functions

(defconst compat-functions
  '(ansi-color-filter-apply
    package--alist
    package--activate-all
    package-activate-all
    package-generate-description-file
    url-file-exists-p)
  "List of function to check Emacs compatibility.")

(dolist (func compat-functions)
  (let ((exists-p))
    (eask-with-progress
      (format "  - Checked function %s... " (ansi-green (format "%s" func)))
      (setq exists-p (fboundp func))
      (if exists-p "done ✓" "missing ✗"))
    (unless exists-p
      (eask-error "Missing function %s in Emacs version %s... " func emacs-version))))

;;
;;; Test variables

(defconst compat-variables
  '(package-quickstart-file)
  "List of variables to check Emacs compatibility.")

(dolist (var compat-variables)
  (let ((exists-p))
    (eask-with-progress
      (format "  - Checked variable %s... " (ansi-green (format "%s" var)))
      (setq exists-p (boundp var))
      (if exists-p "done ✓" "missing ✗"))
    (unless exists-p
      (eask-error "Missing variable %s in Emacs version %s... " var emacs-version))))

;;; compat.el ends here

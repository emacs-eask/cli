;;; compat.el --- Test Emacs compatibility  -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; Test for Emacs' functions and variables existense.
;;
;; See the test result in
;; (https://github.com/emacs-eask/cli/actions/workflows/test-outdated_upgrade.yml).
;;

;;; Code:

(require 'finder)

;;
;;; Test functions

(defconst compat-functions
  '(ansi-color-filter-apply
    ls-lisp-format-file-size
    lsh
    package--alist
    package--activate-all
    package-activate-all
    package-generate-description-file
    directory-empty-p
    url-file-exists-p)
  "List of function to check Emacs compatibility.")

(message "Starting compatibility test for functions...")

(dolist (func compat-functions)
  (let ((exists-p))
    (eask-with-progress
      (format "  - Checked function %s... " (ansi-green func))
      (setq exists-p (fboundp func))
      (if exists-p "done ✓" "missing ✗"))
    (unless exists-p
      (eask-error "Missing function %s in Emacs version %s... " func emacs-version))))

;;
;;; Test variables

(defconst compat-variables
  '(finder-known-keywords
    package-quickstart-file
    print-level
    print-length)
  "List of variables to check Emacs compatibility.")

(message "Starting compatibility test for variables...")

(dolist (var compat-variables)
  (let ((exists-p))
    (eask-with-progress
      (format "  - Checked variable %s... " (ansi-green var))
      (setq exists-p (boundp var))
      (if exists-p "done ✓" "missing ✗"))
    (unless exists-p
      (eask-error "Missing variable %s in Emacs version %s... " var emacs-version))))

;;; compat.el ends here

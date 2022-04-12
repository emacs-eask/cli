;;; compat.el --- Test Emacs compatibility  -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(defconst compat-functions
  '(url-file-exists-p)
  "List of function to check Emacs compatibility.")

(dolist (func compat-functions)
  (let ((exists-p))
    (eask-with-progress
      (format "Checked function %s... " func)
      (setq exists-p (fboundp func))
      (if exists-p "done ✓" "missing ✗"))
    (unless exists-p
      (kill-emacs 1))))

;;; compat.el ends here

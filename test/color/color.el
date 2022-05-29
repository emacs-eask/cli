;;; color.el --- Test to print color on the terminal  -*- lexical-binding: t; -*-

;; Version: 0.0.1
;; Package-Requires: ((emacs "26.1"))

;;; Commentary:
;;; Code:

(let ((eask-verbosity 99)
      (eask-timestamps t)
      (eask-log-level t)
      (eask--ignore-error-p t))
  (eask-debug "This is %s message" (ansi-magenta "DEBUG"))
  (eask-log   "This is %s message" (ansi-magenta "LOG"))
  (eask-info  "This is %s message" (ansi-magenta "INFO"))
  (eask-warn  "This is %s message" (ansi-magenta "WARNING"))
  (eask-error "This is %s message" (ansi-magenta "ERROR")))

;;; color.el ends here

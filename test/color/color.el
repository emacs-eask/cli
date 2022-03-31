;;; color.el --- Test to print color on the terminal  -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(let ((eask-timestamps t)
      (eask-log-level t))
  (eask-debug "This is %s message" (ansi-magenta "DEBUG"))
  (eask-log   "This is %s message" (ansi-magenta "LOG"))
  (eask-info  "This is %s message" (ansi-magenta "INFO"))
  (eask-warn  "This is %s message" (ansi-magenta "WARNING"))
  (eask-error "This is %s message" (ansi-magenta "ERROR")))

;;; color.el ends here

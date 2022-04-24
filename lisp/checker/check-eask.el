;;; check-eask.el --- Run eask checker  -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; Commmand use to run Eask checker
;;
;;   $ eask check-eask
;;

;;; Code:

(load (expand-file-name
       "../_prepare.el"
       (file-name-directory (nth 1 (member "-scriptload" command-line-args))))
      nil t)

(defvar eask--checker-log nil)

(defun eask--load-buffer ()
  "Return the current loading file session."
  (car (cl-remove-if-not
        (lambda (elm) (string-prefix-p " *load*-" (buffer-name elm))) (buffer-list))))

(defun eask--write-log (level msg)
  "Write the log."
  (unless (string= " *temp*" (buffer-name))  ; avoid error from `package-file' directive
    (with-current-buffer (or (eask--load-buffer) (buffer-name))
      (let* ((level-string (cl-case level
                             (`error "Error")
                             (`warn  "Warning")))
             (log (format "%s:%s:%s %s: %s"
                          (file-name-nondirectory (or load-file-name eask-file))
                          (if load-file-name (line-number-at-pos) 0)
                          (if load-file-name (current-column) 0)
                          level-string
                          msg)))
        (push (ansi-color-filter-apply log) eask--checker-log)))))

(add-hook 'eask-on-error-hook #'eask--write-log)
(add-hook 'eask-on-warning-hook #'eask--write-log)

(eask-start
  (if eask--checker-log
      (mapc #'eask-msg (reverse eask--checker-log))
    (eask-msg "(No issues found)")))

;;; check-eask.el ends here

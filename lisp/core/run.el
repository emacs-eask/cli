;;; run.el --- Run the script  -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; Command use to run scripts,
;;
;;   $ eask run [names..]
;;
;;
;;  Initialization options:
;;
;;    [names..]     run the script named <foo>
;;

;;; Code:

(load (expand-file-name
       "../_prepare.el"
       (file-name-directory (nth 1 (member "-scriptload" command-line-args))))
      nil t)

(defun eask--print-scripts ()
  "Print all available scripts."
  (eask-msg "available via `eask run-script`")
  (eask-msg "")
  (let* ((keywords (mapcar #'car (reverse eask-scripts)))
         (offset (eask-seq-str-max keywords))
         (fmt (concat "  %-" (eask-2str offset) "s  %s")))
    (dolist (keyword keywords)
      (eask-msg fmt keyword (cdr (assoc keyword eask-scripts))))
    (eask-msg "")
    (eask-info "(Total of %s available scripts)" (length keywords))))

(defun eask--export-command (command)
  "Export COMMAND instruction."
  (let ((run (expand-file-name "run" eask-homedir)))
    (ignore-errors (make-directory eask-homedir t))  ; generate dir ~/.eask/
    (write-region (concat command "\n") nil run t)
    t))

(eask-start
  (ignore-errors (delete-directory eask-homedir t))  ; clean up
  (cond
   ((null eask-scripts)
    (eask-info "✗ (No scripts specified)")
    (eask-help 'run))
   ((eask-all-p)  ; Run all scripts
    (dolist (data (reverse eask-scripts))
      (eask--export-command (cdr data))))
   ((when-let ((scripts (eask-args)))
      (dolist (script scripts)
        (if-let* ((data (assoc script eask-scripts))
                  (name (car data))
                  (command (cdr data)))
            (eask--export-command command)
          (eask-info "✗ (Missing script: \"%s\")" script)
          (eask-msg "")
          (eask--print-scripts)))))
   (t (eask--print-scripts))))

;;; run.el ends here

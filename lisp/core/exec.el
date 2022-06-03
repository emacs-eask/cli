;;; exec.el --- Execute command with correct load-path set up  -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; Execute command with correct load-path set up
;;
;;   $ eask exec [args..]
;;
;;
;;  Initialization options:
;;
;;    [args..]     execute command with correct load-path set up
;;

;;; Code:

(load (expand-file-name
       "../_prepare.el"
       (file-name-directory (nth 1 (member "-scriptload" command-line-args))))
      nil t)

(defconst eask--homedir (getenv "EASK_HOMEDIR")  ; temporary environment from node
  "Eask temporary storage.")

(defun eask--export-env ()
  "Export environments."
  (let ((epf (expand-file-name "exec-path" eask--homedir))
        (lpf (expand-file-name "load-path" eask--homedir)))
    (ignore-errors (make-directory eask--homedir t))  ; generate dir ~/.eask/
    (write-region (getenv "PATH") nil epf)
    (write-region (getenv "EMACSLOADPATH") nil lpf)))

(eask-start
  (eask-defvc< 27 (eask-pkg-init))  ; XXX: remove this after we drop 26.x
  ;; XXX This is the hack by adding all `bin' folders from local elpa.
  (eask-setup-paths)
  (ignore-errors (delete-directory eask--homedir t))  ; clean up
  (if-let ((name (eask-argv 1)))
      (or
       ;; 1) For Elisp executable (github-elpa)
       (let ((program (executable-find name)))
         (setq commander-args (cddr argv))  ; by pass `--' as well
         (ignore-errors (load program nil t)))
       ;; 2) Export environments, and return back to node for subcommand execution
       (eask-with-progress
         (ansi-green "Exporting environment variables... ")
         (eask--export-env)
         (ansi-green "done ✓")))
    (eask-info "✗ (No exeuction output)")
    (eask-help 'exec)))

;;; exec.el ends here

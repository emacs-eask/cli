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

(defun eask--export-env ()
  "Export environments."
  (let* ((home-dir (getenv "EASK_HOMEDIR"))  ; temporary environment from node
         (epf (expand-file-name "exec-path" home-dir))
         (lpf (expand-file-name "load-path" home-dir)))
    (ignore-errors (make-directory home-dir t))
    (write-region (getenv "PATH") nil epf)
    (write-region (getenv "EMACSLOADPATH") nil lpf)))

(eask-start
  (eask-defvc< 27 (eask-pkg-init))  ; XXX: remove this after we drop 26.x
  ;; XXX This is the hack by adding all `bin' folders from local elpa.
  (eask-setup-paths)
  (setq commander-args (cddr argv))  ; by pass `--' as well
  (if-let ((name (eask-argv 1)))
      (or
       ;; 1) For Elisp executable (github-elpa)
       (let ((program (executable-find name))) (ignore-errors (load program nil t)))
       ;; 2) Export load-path and exec-path
       (eask-with-progress
         (ansi-green "Exporting environment PATHs... ")
         (eask--export-env)
         (ansi-green "done ✓")))
    (eask-info "✗ (No exeuction output)")
    (eask-help 'exec)))

;;; exec.el ends here

;;; core/exec.el --- Execute command with correct PATH set up  -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; Execute command with correct load-path set up
;;
;;   $ eask exec [args..]
;;
;;
;;  Positionals:
;;
;;    [args..]     execute command with correct PATH set up
;;

;;; Code:

(let ((dir (file-name-directory (nth 1 (member "-scriptload" command-line-args)))))
  (load (expand-file-name "_prepare.el"
                          (locate-dominating-file dir "_prepare.el"))
        nil t))

(defconst eask-exec--path-file (expand-file-name "exec-path" eask-homedir)
  "Target file to export the variable `exec-path'.")

(defconst eask-load--path-file (expand-file-name "load-path" eask-homedir)
  "Target file to export the variable `load-path'.")

(defun eask-exec-export-env ()
  "Export environments."
  (ignore-errors (delete-file eask-exec--path-file))
  (ignore-errors (delete-file eask-load--path-file))
  (ignore-errors (make-directory eask-homedir t))  ; generate dir `~/.eask/'
  (write-region (getenv "PATH") nil eask-exec--path-file)
  (write-region (getenv "EMACSLOADPATH") nil eask-load--path-file))

(eask-start
  (eask-defvc< 27 (eask-pkg-init))  ; XXX: remove this after we drop 26.x
  ;; XXX This is the hack by adding all `bin' folders from local elpa.
  (eask-setup-paths)
  (if (eask-argv 1)
      (eask-with-progress
        (ansi-green "Exporting environment variables... ")
        (eask-exec-export-env)
        (ansi-green "done ✓"))
    (eask-info "(No exeuction output)")
    (eask-help "core/exec")))

;;; core/exec.el ends here

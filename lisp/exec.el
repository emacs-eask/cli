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

(load-file (expand-file-name
            "_prepare.el"
            (file-name-directory (nth 1 (member "-scriptload" command-line-args)))))

(eask-start
  (eask-pkg-init)
  (setq commander-args (cddr argv))  ; by pass `--' as well
  (if-let* ((command (eask-argv 1))
            (exe (executable-find command)))
      (load-file exe)
    (error "Executable `%s`.. not found" command)))

;;; exec.el ends here

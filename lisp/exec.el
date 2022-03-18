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

(defun eask--add-bin-exec-path ()
  "Add all bin directory to `exec-path'."
  (dolist (filename (directory-files-recursively package-user-dir "^\\([^.]\\|\\.[^.]\\|\\.\\..\\)"))
    (when (string-suffix-p "bin/" (file-name-directory filename))
      (add-to-list 'exec-path (file-name-directory filename))))
  (delete-dups exec-path))

(eask-start
  (eask-pkg-init)
  (eask--add-bin-exec-path)
  (setq commander-args (cddr argv))  ; by pass `--' as well
  (if-let* ((command (eask-argv 1))
            (exe (executable-find command)))
      (load-file exe)
    (message "Executable `%s`.. not found" command)))

;;; exec.el ends here

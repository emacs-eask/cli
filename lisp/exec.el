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
       "_prepare.el"
       (file-name-directory (nth 1 (member "-scriptload" command-line-args))))
      nil t)

(eask-start
  (eask-pkg-init)
  (eask-install-dependencies)
  (setq commander-args (cddr argv))  ; by pass `--' as well
  (let* ((program (eask-argv 1))
         (exe (executable-find program))
         (command (mapconcat #'identity (append (list program) commander-args) " ")))
    (unless (or (ignore-errors (load exe t t))
                (progn
                  (eask-info "Execute command %s..." command)
                  (zerop (shell-command command))))
      (error "Error from execution."))))

;;; exec.el ends here

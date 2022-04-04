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

(defun eask--help-exec ()
  "Print help if command failed."
  (eask-msg "")
  (eask-msg "You would need to specify the program name in order to make execution!")
  (eask-msg "")
  (eask-msg "  $ eask exec [program] [options..]"))

(eask-start
  (eask-pkg-init)
  ;; XXX This is the hack by adding all `bin' folders from local elpa.
  (eask-setup-paths)
  (setq commander-args (cddr argv))  ; by pass `--' as well
  (if-let* ((program (eask-argv 1))
            (command (mapconcat #'identity (append (list program) commander-args) " ")))
      (unless (ignore-errors (load (executable-find program) t t))
        (if (progn
              (eask-info "Execute command %s..." command)
              (zerop (shell-command command "*output*" "*error*")))
            (with-current-buffer "*output*" (eask-msg (buffer-string)))
          (with-current-buffer "*error*" (eask-msg (buffer-string)))
          (error "Error from the execution.")))
    (eask-info "✗ (No exeuction output)")
    (eask--help-exec)))

;;; exec.el ends here

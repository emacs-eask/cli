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

(defun eask--shell-command (command)
  "Wrap `shell-command' with better output to terminal."
  (eask-info "Execute command %s..." command)
  (let ((code (shell-command command "*output*" "*error*")))
    (if (zerop code)
        (with-current-buffer "*output*" (eask-msg (buffer-string)))
      (with-current-buffer "*error*" (eask-msg (ansi-red (buffer-string))))
      (error "Error from the execution, exit code %s" code))))

(eask-start
  (eask-pkg-init)
  ;; XXX This is the hack by adding all `bin' folders from local elpa.
  (eask-setup-paths)
  (setq commander-args (cddr argv))  ; by pass `--' as well
  (if-let* ((program (eask-argv 1))
            (program (or (executable-find program) program))
            (command (mapconcat #'identity (append (list program) commander-args) " ")))
      (or (ignore-errors (load program t t))
          (eask--shell-command command))
    (eask-info "✗ (No exeuction output)")
    (eask--help-exec)))

;;; exec.el ends here

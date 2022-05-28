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

(defun eask--shell-command (command)
  "Wrap `shell-command' with better output to terminal."
  (eask-info "Executing command: %s" command)
  (let ((code (shell-command command "*output*" "*error*")))
    (if (zerop code)
        (eask-with-verbosity 'debug
          (with-current-buffer "*output*" (eask-msg (buffer-string))))
      (with-current-buffer "*error*" (eask-msg (ansi-red (buffer-string))))
      (eask-error "Error from the execution, exit code %s" code))))

(eask-start
  (eask-defvc< 27 (eask-pkg-init))  ; XXX: remove this after we drop 26.x
  ;; XXX This is the hack by adding all `bin' folders from local elpa.
  (eask-setup-paths)
  (setq commander-args (cddr eask-argv))  ; by pass `--' as well
  (if-let ((name (eask-argv 1)))
      (let* ((program (or (executable-find name) name))
             (command (mapconcat #'identity (append (list program) commander-args) " ")))
        (eask--shell-command command))
    (eask-info "✗ (No exeuction output)")
    (eask-help 'exec)))

;;; exec.el ends here

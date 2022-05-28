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
  (eask-info "Start command: %s" command)
  (let ((code (eask--silent (shell-command command "*output*" "*error*"))))
    (if (zerop code)
        (with-current-buffer "*output*" (eask-msg (buffer-string)))
      (with-current-buffer "*error*" (eask-msg (ansi-red (buffer-string))))
      (eask-error "Error from the execution, exit code %s" code))))

(eask-start
  (eask-defvc< 27 (eask-pkg-init))  ; XXX: remove this after we drop 26.x
  ;; XXX This is the hack by adding all `bin' folders from local elpa.
  (eask-setup-paths)
  (if-let ((name (eask-argv 1)))
      (or
       ;; 1) For Elisp executable (github-elpa)
       (let ((program (executable-find name))) (ignore-errors (load program t t)))
       ;; 2) Execute `shell-command'
       (let* ((program (or (executable-find name) name))
              (commands (cddr (eask-args)))
              (command (mapconcat #'identity (append (list program) commands) " ")))
         (eask--shell-command command)))
    (eask-info "✗ (No exeuction output)")
    (eask-help 'exec)))

;;; exec.el ends here

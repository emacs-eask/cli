;;; run/command.el --- Run custom command  -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; Command use to run custom command
;;
;;   $ eask run command [names..]
;;
;;
;;  Positionals:
;;
;;    [names..]     name of the function command
;;

;;; Code:

(let ((dir (file-name-directory (nth 1 (member "-scriptload" command-line-args)))))
  (load (expand-file-name "_prepare.el"
                          (locate-dominating-file dir "_prepare.el"))
        nil t))

(defun eask-run-command--desc (name)
  "Return command's description by its command's NAME."
  (car (split-string (or (documentation name) "") "\n")))

(defun eask-run-command--print-commands ()
  "Print all available commands."
  (eask-msg "available via `eask run command`")
  (eask-msg "")
  (let* ((keys (reverse eask-commands))
         (offset (eask-seq-str-max keys))
         (fmt (concat "  %-" (eask-2str offset) "s  %s")))
    (dolist (key keys)
      (eask-msg fmt key (eask-run-command--desc key)))
    (eask-msg "")
    (eask-info "(Total of %s available script%s)" (length keys)
               (eask--sinr keys "" "s"))))

(defun eask-run-command--execute (name)
  "Execute the command by NAME."
  (eask-info "[RUN]: %s" name)
  (funcall (eask-intern name)))

(defun eask-run-command--unmatched-commands (commands)
  "Return a list of COMMANDS that cannot be found in `eask-commands'."
  (let (unmatched)
    (dolist (command commands)
      (unless (memq (eask-intern command) eask-commands)
        (push command unmatched)))
    unmatched))

(eask-start
  (cond ((null eask-commands)
         (eask-info "(No command specified)")
         (eask-help "run/command"))
        ((eask-all-p)
         (dolist (name (reverse eask-commands))
           (eask-run-command--execute name)))
        ((when-let* ((commands (eask-args)))
           (if-let* ((unmatched (eask-run-command--unmatched-commands commands)))
               (progn  ; if there are unmatched commands, don't even try to execute
                 (eask-info "(Missing command%s: `%s`)"
                            (eask--sinr unmatched "" "s")
                            (mapconcat #'identity unmatched ", "))
                 (eask-msg "")
                 (eask-run-command--print-commands))
             (dolist (command commands)
               (eask-run-command--execute command))
             t)))
        (t (eask-run-command--print-commands))))

;;; run/command.el ends here

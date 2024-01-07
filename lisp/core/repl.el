;;; core/repl.el --- Start the Elisp REPL  -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; Command use to start the Elisp REPL,
;;
;;   $ eask repl
;;

;;; Code:

(let ((dir (file-name-directory (nth 1 (member "-scriptload" command-line-args)))))
  (load (expand-file-name "_prepare.el"
                          (locate-dominating-file dir "_prepare.el"))
        nil t))

(defvar eask--repl-old-pos nil
  "Record the last position to output on the screen for the `ielm' buffer.")

(defun eask--repl-output ()
  "Print the REPL result to screen."
  (unless eask--repl-old-pos (setq eask--repl-old-pos (point-min)))
  (with-current-buffer "*ielm*"
    (goto-char eask--repl-old-pos)
    (while (not (eobp))
      (let ((line (thing-at-point 'line t)))
        (unless (string-prefix-p "ELISP> " line)
          (eask-print line)))
      (forward-line 1)
      (end-of-line))
    ;; Record last output position
    (setq eask--repl-old-pos (point))))

(eask-start
  (require 'ielm)
  (ielm)
  (eask--repl-output)
  (let ((input))
    (while (setq input (read-from-minibuffer (ansi-blue "ELISP> ")))
      (with-current-buffer "*ielm*"
        (insert input)
        (eask--silent (ielm-send-input))
        (eask--repl-output)))))

;;; core/repl.el ends here

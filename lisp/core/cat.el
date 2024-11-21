;;; core/cat.el --- View filename(s)  -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; Command use to view filename(s)
;;
;;   $ eask cat <patterns..>
;;
;;
;;  Positionals:
;;
;;    <patterns..>     filename(s) to view
;;
;;  Optional arguments:
;;
;;    --number, -n       view with line numbers
;;

;;; Code:

(let ((dir (file-name-directory (nth 1 (member "-scriptload" command-line-args)))))
  (load (expand-file-name "_prepare.el"
                          (locate-dominating-file dir "_prepare.el"))
        nil t))

(eask-start
  ;; Preparation
  (eask-archive-install-packages '("gnu" "melpa")
                                 'e2ansi)
  (eask-msg "")

  ;; Start the task
  (require 'e2ansi)
  (if-let* ((patterns (eask-args))
            (filenames (eask-expand-file-specs patterns)))
      (dolist (filename filenames)
        (eask-info "[+] %s" filename)
        (with-current-buffer (find-file filename)
          (ignore-errors (font-lock-ensure))
          (goto-char (point-min))
          (let* ((max-line (save-excursion (line-number-at-pos (point-max))))
                 (max-line (eask-2str max-line))
                 (offset (eask-2str (length max-line)))
                 (line-no 1))
            (while (not (eobp))
              (let* ((line (buffer-substring (line-beginning-position) (line-end-position)))
                     (line (if ansi-inhibit-ansi line (e2ansi-string-to-ansi line))))
                (if (eask-number-p)
                    (eask-println (concat "%" offset "d  %s") line-no line)
                  (eask-println "%s" line)))
              (forward-line 1)
              (cl-incf line-no)))))
    (eask-info "(No files match wildcard: %s)"
               (mapconcat #'identity patterns " "))))

;;; core/cat.el ends here

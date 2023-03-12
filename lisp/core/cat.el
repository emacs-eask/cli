;;; core/cat.el --- Byte compile all Emacs Lisp files in the package  -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; Byte compile all Emacs Lisp files in the package
;;
;;   $ eask cat [names..]
;;
;;
;;  Positional arguments:
;;
;;    [filenames..]     filename(s) to view
;;
;;  Optional arguments:
;;
;;    [number]
;;

;;; Code:

(let ((dir (file-name-directory (nth 1 (member "-scriptload" command-line-args)))))
  (load (expand-file-name "_prepare.el"
                          (locate-dominating-file dir "_prepare.el"))
        nil t))

(eask-start
  ;; Preparation
  (eask-with-archives "melpa"
    (eask-package-install 'e2ansi))
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
                 (line 1))
            (while (not (eobp))
              (message (concat "%" offset "s  %s")
                       (if (eask-number-p) line "")
                       (e2ansi-string-to-ansi (buffer-substring (line-beginning-position) (line-end-position))))
              (forward-line 1)
              (cl-incf line)))))
    (eask-info "(No files match wildcard: %s)" patterns)))

;;; core/cat.el ends here

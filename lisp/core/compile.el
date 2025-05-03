;;; core/compile.el --- Byte-compile `.el' files  -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; Byte-compile `.el' files,
;;
;;   $ eask compile [names..]
;;
;;
;;  Positionals:
;;
;;    [names..]     specify files to byte-compile
;;

;;; Code:

(let ((dir (file-name-directory (nth 1 (member "-scriptload" command-line-args)))))
  (load (expand-file-name "_prepare.el"
                          (locate-dominating-file dir "_prepare.el"))
        nil t))

;;
;;; Flags

(advice-add #'eask-allow-error-p :override #'eask-always)

;;
;;; Handle options

(add-hook 'eask-before-command-hook
          (lambda ()
            (when (eask-strict-p)
              (setq byte-compile-error-on-warn t))
            (when (eask-reach-verbosity-p 'debug)
              (setq byte-compile-verbose t))))

;;
;;; Core

(require 'bytecomp)

;; XXX: The function `byte-compile-warn' was last modified in 2015;
;; I'll say it's safe to override this function.
(advice-add 'byte-compile-warn :override
            (lambda (format &rest args)
              (setq format (apply #'format-message format args))
              (byte-compile-log-warning format t (if byte-compile-error-on-warn
                                                     :error
                                                   :warning))))

(defun eask-compile--print-log ()
  "Print `*Compile-Log*' buffer."
  (when (get-buffer byte-compile-log-buffer)
    (with-current-buffer byte-compile-log-buffer
      (if (and (eask-clean-p) (eask-strict-p))
          (eask-error (buffer-string))  ; Exit with error code!
        (eask-print-log-buffer))
      (eask-msg ""))))

(defun eask-compile--byte-compile-file-external-content (filename cmd)
  "Extract result after executing byte-compile the FILENAME.

The CMD is the command to start a new Emacs session."
  (with-temp-buffer
    (insert (shell-command-to-string cmd))
    (goto-char (point-min))
    (search-forward filename nil t)
    (re-search-forward "[ \t\r\n]" nil t)
    (let ((line (string-trim (thing-at-point 'line))))
      (if (and (string-prefix-p "Compiling " line)
               (or (string-match-p "... skipped" line)
                   (string-match-p "... done" line)))
          (delete-region (point-min) (line-end-position 1))
        (delete-region (point-min) (point))))
    (when (search-forward "(Total of " nil t)
      (goto-char (point-max))
      (delete-region (line-beginning-position -1) (point-max)))
    (string-trim (buffer-string))))

(defun eask-compile--byte-compile-file-external (filename)
  "Byte compile FILENAME with clean environment by opening a new Emacs session."
  (let* ((cmd (split-string eask-invocation "\n" t))
         (cmd (format "\"%s\""(mapconcat #'identity cmd "\" \"")))
         (args (eask-args))
         (argv (cl-remove-if
                (lambda (arg)
                  (or (string= "--clean" arg)  ; prevent infinite call
                      (member arg args)))      ; remove repeated arguments
                (eask-argv-out)))
         (args (append `(,(eask-command) ,(concat "\"" filename "\"")) argv))
         (args (mapconcat #'identity args " "))
         (cmd (concat cmd " " args))
         (content (eask-compile--byte-compile-file-external-content filename cmd)))
    (if (string-empty-p content)
        t  ; no error, good!
      (with-current-buffer (get-buffer-create byte-compile-log-buffer)
        (insert content)))))

(defun eask-compile--byte-compile-file (filename)
  "Byte compile FILENAME."
  ;; *Compile-Log* does not kill itself. Make sure it's clean before we do
  ;; next byte-compile task.
  (ignore-errors (kill-buffer byte-compile-log-buffer))
  (let* ((filename (expand-file-name filename))
         (result))
    (eask-with-progress
      (unless byte-compile-verbose (format "Compiling %s... " filename))
      (eask-with-verbosity 'debug
        (setq result (if (eask-clean-p)
                         (eask-compile--byte-compile-file-external filename)
                       (byte-compile-file filename))
              result (eq result t)))
      (unless byte-compile-verbose (if result "done ✓" "skipped ✗")))
    (eask-compile--print-log)
    result))

(defun eask-compile--files (files)
  "Compile sequence of FILES."
  (let* ((compiled (cl-remove-if-not #'eask-compile--byte-compile-file files))
         (compiled (length compiled))
         (skipped (- (length files) compiled)))
    ;; XXX: Avoid last newline from the log buffer!
    (unless (get-buffer byte-compile-log-buffer)
      (eask-msg ""))
    (eask-info "(Total of %s file%s compiled, %s skipped)" compiled
               (eask--sinr compiled "" "s")
               skipped)))

(eask-start
  (eask-defvc< 27 (eask-pkg-init))  ; XXX: remove this after we drop 26.x
  (let* ((patterns (eask-args))
         (files (if patterns
                    (eask-expand-file-specs patterns)
                  (eask-package-el-files))))
    (cond
     ;; Files found, do the action!
     (files
      (eask-compile--files files))
     ;; Pattern defined, but no file found!
     (patterns
      (eask-info "(No files match wildcard: %s)"
                 (mapconcat #'identity patterns " ")))
     ;; Default, print help!
     (t
      (eask-info "(No files have been compiled)")
      (eask-help "core/compile")))))

;;; core/compile.el ends here

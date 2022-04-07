;;; compile.el --- Byte compile all Emacs Lisp files in the package  -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; Byte compile all Emacs Lisp files in the package
;;
;;   $ eask compile [names..]
;;
;;
;;  Initialization options:
;;
;;    [names..]     specify files to byte-compile
;;

;;; Code:

(load (expand-file-name
       "_prepare.el"
       (file-name-directory (nth 1 (member "-scriptload" command-line-args))))
      nil t)

(defun eask--help-compile ()
  "Print help if command failed"
  (eask-msg "")
  (eask-msg "💡 You need to specify file(s) you want to compile")
  (eask-msg "")
  (eask-msg "    $ eask %s FILE-1 FILE-2" (eask-command))
  (eask-msg "")
  (eask-msg "💡 Or edit Eask file with [files] specifier")
  (eask-msg "")
  (eask-msg "   [+] (files \"FILE-1\" \"FILE-2\")")
  (eask-msg ""))

;; Handle options
(add-hook 'eask-before-command-hook
          (lambda ()
            (when (eask-strict-p) (setq byte-compile-error-on-warn t))
            (when (= eask-verbosity 4) (setq byte-compile-verbose t))))

(defconst eask-compile-log-buffer-name "*Compile-Log*"
  "Byte-compile log buffer name.")

(defun eask--print-compile-log ()
  "Print `*Compile-Log*' buffer."
  (when (get-buffer eask-compile-log-buffer-name)
    (with-current-buffer eask-compile-log-buffer-name
      (eask-print-log-buffer)
      (message ""))))

(defun eask--byte-compile-file (filename)
  "Byte compile FILENAME."
  ;; *Compile-Log* does not kill itself. Make sure it's clean before we do
  ;; next byte-compile task.
  (ignore-errors (kill-buffer eask-compile-log-buffer-name))
  (let* ((filename (expand-file-name filename))
         (result))
    (eask-with-progress
      (unless byte-compile-verbose (format "Compiling %s... " filename))
      (eask-with-verbosity 'debug
        (setq result (byte-compile-file filename)
              result (eq result t)))
      (if result "done ✓" "skipped ✗"))
    (eask--print-compile-log)
    result))

(defun eask--compile-files (files)
  "Compile sequence of files."
  (let* ((compiled (cl-remove-if-not #'eask--byte-compile-file files))
         (compiled (length compiled))
         (skipped (- (length files) compiled)))
    (eask-info "(Total of %s file%s compiled, %s skipped)" compiled
               (eask--sinr compiled "" "s")
               skipped)))

(eask-start
  (eask-pkg-init)
  (if-let ((files (or (eask-args) (eask-package-el-files))))
      (eask--compile-files files)
    (eask-info "(No files have been compiled)")
    (eask--help-compile)))

;;; compile.el ends here

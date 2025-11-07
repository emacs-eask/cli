;;; core/analyze.el --- Run eask checker  -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; Commmand use to run Eask checker
;;
;;   $ eask analyze [FILES..]
;;
;;
;;  Positionals:
;;
;;    [files..]     specify Eask-files for checker to lint
;;
;;  Optional arguments:
;;
;;    --json        Output lint result in JSON format
;;

;;; Code:

(let ((dir (file-name-directory (nth 1 (member "-scriptload" command-line-args)))))
  (load (expand-file-name "_prepare.el"
                          (locate-dominating-file dir "_prepare.el"))
        nil t))

;; Plain Text
(defvar eask-analyze--log nil)
;; JSON format
(defvar eask-analyze--warnings nil)
(defvar eask-analyze--errors nil)

;; Warning flag
(defvar eask-analyze--warning-p nil)
;; Error flag
(defvar eask-analyze--error-p nil)

(defun eask-analyze--pretty-json (json)
  "Return pretty JSON."
  (with-temp-buffer (insert json) (json-pretty-print-buffer) (buffer-string)))

(defun eask-analyze--load-buffer ()
  "Return the current file loading session."
  (car (cl-remove-if-not (lambda (elm)
                           (string-prefix-p " *load*-" (buffer-name elm)))
                         (buffer-list))))

(defun eask-analyze--write-json-format (level msg)
  "Prepare log for JSON format.

For arguments LEVEL and MSG, please see function `eask-analyze--write-log' for more
information."
  (let* ((bounds (bounds-of-thing-at-point 'sexp))
         (filename (or load-file-name eask-file))
         (start (car bounds))
         (end (cdr bounds))
         (start-line (if load-file-name (line-number-at-pos start) 0))
         (start-col  (if load-file-name (eask--column-at-point start) 0))
         (start-pos  (if load-file-name start 0))
         (end-line   (if load-file-name (line-number-at-pos end) 0))
         (end-col    (if load-file-name (eask--column-at-point end) 0))
         (end-pos    (if load-file-name end 0))
         (msg (ansi-color-filter-apply msg)))
    (push `((range . ((start . ((line . ,start-line)
                                (col  . ,start-col)
                                (pos  . ,start-pos)))
                      (end . ((line . ,end-line)
                              (col  . ,end-col)
                              (pos  . ,end-pos)))))
            (filename . ,filename)
            (message  . ,msg))
          (cl-case level
            (`error eask-analyze--errors)
            (`warn  eask-analyze--warnings)))))

(defun eask-analyze--write-plain-text (level msg)
  "Prepare log for plain text format.

For arguments LEVEL and MSG, please see function `eask-analyze--write-log' for more
information."
  (let* ((level-string (cl-case level
                         (`error "Error")
                         (`warn  "Warning")))
         (log (format "%s:%s:%s %s: %s"
                      (or load-file-name eask-file)
                      (if load-file-name (line-number-at-pos) 0)
                      (if load-file-name (current-column) 0)
                      level-string
                      msg)))
    (push (ansi-color-filter-apply log) eask-analyze--log)))

(defun eask-analyze--write-log (level msg)
  "Write the log.

Argument LEVEL and MSG are data from the debug log signal."
  (unless (string= " *temp*" (buffer-name))  ; avoid error from `package-file' directive
    (when (eq 'error level)
      (setq eask-analyze--error-p t))
    (when (eq 'warn level)
      (setq eask-analyze--warning-p t))
    (with-current-buffer (or (eask-analyze--load-buffer) (buffer-name))
      (funcall
       (cond ((eask-json-p) #'eask-analyze--write-json-format)
             (t             #'eask-analyze--write-plain-text))
       level msg))))

(defun eask-analyze--file (files)
  "Lint list of Eask FILES."
  (let (checked-files content)
    ;; Linting
    (dolist (file files)
      (eask--silent-error
        (eask--save-load-eask-file file
            (push file checked-files)
          ;; also count files with errors in the total count
          (push file checked-files))))

    ;; Print result
    (eask-msg "")
    (cond ((eask-json-p)  ; JSON format
           ;; Fill content with result.
           (when (or eask-analyze--warnings eask-analyze--errors)
             (setq content
                   (eask-analyze--pretty-json (json-encode
                                               `((warnings . ,eask-analyze--warnings)
                                                 (errors   . ,eask-analyze--errors))))))
           ;; XXX: When printing the result, no color allow.
           (eask--with-no-color
             (eask-msg (or content "{}"))))
          (eask-analyze--log  ; Plain text
           (setq content
                 (with-temp-buffer
                   (dolist (msg (reverse eask-analyze--log))
                     (insert msg "\n"))
                   (buffer-string)))
           ;; XXX: When printing the result, no color allow.
           (eask--with-no-color
             (mapc #'eask-msg (reverse eask-analyze--log)))))

    (eask-info "(Checked %s file%s)"
               (length checked-files)
               (eask--sinr checked-files "" "s"))

    ;; Output file
    (when (and content (eask-output))
      (write-region content nil (eask-output)))))

;;
;;; Program Entry

;; Preparation
(add-hook 'eask-on-error-hook #'eask-analyze--write-log)
(add-hook 'eask-on-warning-hook #'eask-analyze--write-log)

(let* ((default-directory (cond ((eask-global-p) eask-homedir)
                                ((eask-config-p) user-emacs-directory)
                                (t default-directory)))
       (patterns (eask-args))
       (files (if patterns
                  (eask-expand-file-specs patterns)
                (eask-expand-file-specs '("Eask*" "**/Eask*")))))
  (cond
   ;; Files found, do the action!
   (files
    (eask-analyze--file files)
    (when (or eask-analyze--error-p
              ;; strict flag turns warnings into errors
              (and eask-analyze--warning-p (eask-strict-p)))
      (eask--exit 'failure)))
   ;; Pattern defined, but no file found!
   (patterns
    (eask-info "(No files match wildcard: %s)"
               (mapconcat #'identity patterns " ")))
   ;; Default, print help!
   (t
    (eask-info "(No Eask-files have been checked)")
    (eask-help "core/analyze"))))

;;; core/analyze.el ends here

;;; checker/check-eask.el --- Run eask checker  -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; Commmand use to run Eask checker
;;
;;   $ eask check-eask [FILES..]
;;
;;
;;  Initialization options:
;;
;;    [files..]     specify Eask-files for checker to lint
;;    --json        Output lint result in JSON format
;;

;;; Code:

(load (expand-file-name
       "../_prepare.el"
       (file-name-directory (nth 1 (member "-scriptload" command-line-args))))
      nil t)

;; Plain Text
(defvar eask--checker-log nil)
;; JSON format
(defvar eask--checker-warnings nil)
(defvar eask--checker-errors nil)

(defun eask--pretty-json (json)
  "Return pretty JSON."
  (with-temp-buffer (insert json) (json-pretty-print-buffer) (buffer-string)))

(defun eask--column-at-point (point)
  "Get column at POINT."
  (save-excursion (goto-char point) (current-column)))

(defun eask--load-buffer ()
  "Return the current file loading session."
  (car (cl-remove-if-not
        (lambda (elm) (string-prefix-p " *load*-" (buffer-name elm))) (buffer-list))))

(defun eask--write-json-format (level msg)
  "Prepare log for JSON format."
  (let* ((thing (thing-at-point 'sexp))
         (bounds (bounds-of-thing-at-point 'sexp))
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
            (message . ,msg))
          (cl-case level
            (`error eask--checker-errors)
            (`warn  eask--checker-warnings)))))

(defun eask--write-plain-text (level msg)
  "Prepare log for plain text format."
  (let* ((level-string (cl-case level
                         (`error "Error")
                         (`warn  "Warning")))
         (log (format "%s:%s:%s %s: %s"
                      (or load-file-name eask-file)
                      (if load-file-name (line-number-at-pos) 0)
                      (if load-file-name (current-column) 0)
                      level-string
                      msg)))
    (push (ansi-color-filter-apply log) eask--checker-log)))

(defun eask--write-log (level msg)
  "Write the log."
  (unless (string= " *temp*" (buffer-name))  ; avoid error from `package-file' directive
    (with-current-buffer (or (eask--load-buffer) (buffer-name))
      (funcall
       (cond ((eask-json-p) #'eask--write-json-format)
             (t             #'eask--write-plain-text))
       level msg))))

(defun eask--check-file (files)
  "Lint list of Eask FILES."
  (let (checked-files content)
    ;; Linting
    (dolist (file files)
      (eask--save-load-eask-file file
          (push file checked-files)))

    ;; Print result
    (eask-msg "")
    (cond ((and (eask-json-p)  ; JSON format
                (or eask--checker-warnings eask--checker-errors))
           (setq content
                 (eask--pretty-json (json-encode
                                     `((warnings . ,eask--checker-warnings)
                                       (errors   . ,eask--checker-errors)))))
           (eask-msg content))
          (eask--checker-log  ; Plain text
           (setq content
                 (with-temp-buffer
                   (dolist (msg (reverse eask--checker-log))
                     (insert msg "\n"))
                   (buffer-string)))
           (mapc #'eask-msg (reverse eask--checker-log)))
          (t
           (eask-info "(Checked %s file%s)"
                      (length checked-files)
                      (eask--sinr checked-files "" "s"))))

    ;; Output file
    (when (and content (eask-output))
      (write-region content nil (eask-output)))))

;;
;;; Program Entry

;; Preparation
(add-hook 'eask-on-error-hook #'eask--write-log)
(add-hook 'eask-on-warning-hook #'eask--write-log)

(let* ((default-directory (if (eask-global-p) user-emacs-directory
                            default-directory))
       (patterns (eask-args))
       (files (if patterns
                  (eask-expand-file-specs patterns)
                (eask-expand-file-specs '("Eask*" "**/Eask*")))))
  (cond
   ;; Files found, do the action!
   (files
    (eask--check-file files))
   ;; Pattern defined, but no file found!
   (patterns
    (eask-info "No files found with wildcard pattern: %s"
               (mapconcat #'identity patterns " ")))
   ;; Default, print help!
   (t
    (eask-info "(No Eask-files have been checked)")
    (eask-help "checker/check-eask"))))

;;; checker/check-eask.el ends here

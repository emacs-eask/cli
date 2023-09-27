;;; init/source.el --- Initialize Eask from elisp source  -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; Commmand use to convert elisp-file to Eask-file
;;
;;   $ eask init --from source
;;

;;; Code:

(let ((dir (file-name-directory (nth 1 (member "-scriptload" command-line-args)))))
  (load (expand-file-name "_prepare.el"
                          (locate-dominating-file dir "_prepare.el"))
        nil t))

(defun eask--convert-source (filename)
  "Convert elisp source FILENAME to Eask."
  (let* ((filename (expand-file-name filename))
         (file (file-name-nondirectory (eask-root-del filename)))
         (new-file (concat "Eask." (file-name-sans-extension file)))
         (new-filename (expand-file-name new-file))
         (pkg-desc (with-temp-buffer
                     (insert-file-contents filename)
                     (eask-ignore-errors-silent (package-buffer-info))))  ; Read it!
         (converted))
    (eask-with-progress
      (format "Converting file `%s` to `%s`... " file new-file)
      (eask-with-verbosity 'debug
        (cond ((not (string-suffix-p ".el" file))
               (eask-debug "✗ Invalid elisp filename, the file should end with `.el`"))
              ((file-exists-p new-filename)
               (eask-debug "✗ The file `%s` already presented" new-file))
              (t
               (when pkg-desc
                 (with-current-buffer (find-file new-filename)
                   (goto-char (point-min))

                   (let* ((eask-package-desc pkg-desc)
                          (package-name (package-desc-name pkg-desc))
                          (version (package-desc-version pkg-desc))
                          (version (package-version-join version))
                          (description (package-desc-summary pkg-desc))
                          (website (eask-package-desc-url))
                          (keywords (eask-package-desc-keywords))
                          (keywords (string-join keywords "\" \""))
                          (reqs (package-desc-reqs pkg-desc))
                          (content (format
                                    "(package \"%s\"
         \"%s\"
         \"%s\")

(website-url \"%s\")
(keywords \"%s\")

(package-file \"%s\")

(script \"test\" \"echo \\\"Error: no test specified\\\" && exit 1\")

(source \"gnu\")

"
                                    package-name version description website keywords
                                    file)))
                     (insert content)

                     (dolist (req reqs)
                       (let* ((req-name (car req))
                              (req-version (cdr req))
                              (req-version (package-version-join (car req-version))))
                         (insert (format "(depends-on \"%s\" \"%s\")\n" req-name req-version)))))

                   (save-buffer))
                 (setq converted t)))))
      (if converted "done ✓" "skipped ✗"))
    converted))

(eask-start
  (let* ((patterns (eask-args))
         (files (if patterns
                    (eask-expand-file-specs patterns)
                  (directory-files default-directory t ".el")))
         (files (cl-remove-if-not (lambda (file)
                                    (string-suffix-p ".el" (file-name-nondirectory file)))
                                  files))
         (converted 0))
    (cond
     ;; Files found, do the action!
     (files
      (dolist (file files)
        (when (eask--convert-source file)
          (cl-incf converted)))
      (eask-msg "")
      (eask-info "(Total of %s elisp file%s converted)" converted
                 (eask--sinr converted "" "s")))
     ;; Pattern defined, but no file found!
     (patterns
      (eask-info "(No files match wildcard: %s)"
                 (mapconcat #'identity patterns " ")))
     ;; Default, print help!
     (t
      (eask-info "(No elisp files have been converted to Eask)")
      (eask-help "init/source")))))

;;; init/source.el ends here

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

(defun eask-init-source--convert (filename)
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
              (t
               (when pkg-desc
                 (with-current-buffer (find-file new-filename)
                   (erase-buffer)
                   (goto-char (point-min))

                   (let* ((eask-package-desc pkg-desc)
                          (package-name (package-desc-name pkg-desc))
                          (version (package-desc-version pkg-desc))
                          (version (package-version-join version))
                          (description (package-desc-summary pkg-desc))
                          (description (prin1-to-string description))
                          (website (eask-package-desc-url))
                          (keywords (eask-package-desc-keywords))
                          (keywords (string-join keywords "\" \""))
                          (reqs (package-desc-reqs pkg-desc))
                          (content (format
                                    "(package \"%s\"
         \"%s\"
         %s)

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
    (when converted new-filename)))

(eask-start
  (let* ((patterns (eask-args))
         (files (if patterns
                    (eask-expand-file-specs patterns)
                  (directory-files default-directory t ".el")))
         (files (cl-remove-if-not (lambda (file)
                                    (string-suffix-p ".el" (file-name-nondirectory file)))
                                  files))
         (converted-files))
    (cond
     ;; Files found, do the action!
     (files
      (dolist (file files)
        (when-let ((new-filename (eask-init-source--convert file)))
          (push new-filename converted-files)))
      ;; Automatically rename file into Eask file when only one file is converted!
      (when (= (length converted-files) 1)
        (rename-file (car converted-files) "Eask" t))
      (eask-msg "")
      (eask-info "(Total of %s elisp file%s converted)" (length converted-files)
                 (eask--sinr converted-files "" "s")))
     ;; Pattern defined, but no file found!
     (patterns
      (eask-info "(No files match wildcard: %s)"
                 (mapconcat #'identity patterns " ")))
     ;; Default, print help!
     (t
      (eask-info "(No elisp files have been converted to Eask)")
      (eask-help "init/source")))))

;;; init/source.el ends here

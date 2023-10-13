;;; init/eldev.el --- Initialize Eask from Eldev  -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; Commmand use to convert Eldev-file to Eask-file
;;
;;   $ eask init --from eldev
;;

;;; Code:

(let ((dir (file-name-directory (nth 1 (member "-scriptload" command-line-args)))))
  (load (expand-file-name "_prepare.el"
                          (locate-dominating-file dir "_prepare.el"))
        nil t))

(defun eask--eldev-map-elpa (name)
  "Convert Eldev mapping to Eask mapping."
  (pcase name
    ("melpa-unstable" 'melpa)
    (_ name)))

(defun eask--convert-eldev (filename)
  "Convert Eldev FILENAME to Eask."
  (let* ((filename (expand-file-name filename))
         (file (file-name-nondirectory (eask-root-del filename)))
         (new-file (eask-s-replace "Eldev" "Eask" file))
         (new-filename (expand-file-name new-file))
         (converted))
    (eask-with-progress
      (format "Converting file `%s` to `%s`... " file new-file)
      (eask-with-verbosity 'debug
        (cond ((not (string-prefix-p "Eldev" file))
               (eask-debug "✗ Invalid Eldev filename, the file should start with `Eldev`"))
              (t
               (with-current-buffer (find-file new-filename)
                 (erase-buffer)
                 (goto-char (point-min))

                 (let* ((project-name (file-name-nondirectory (directory-file-name default-directory)))
                        (package-name (eask-read-string (format "\npackage name: (%s) " project-name) nil nil project-name))
                        (version (eask-read-string "version: (1.0.0) " nil nil "1.0.0"))
                        (description (eask-read-string "description: "))
                        (guess-entry-point (eask-guess-entry-point project-name))
                        (entry-point (eask-read-string (format "entry point: (%s) " guess-entry-point)
                                                       nil nil guess-entry-point))
                        (emacs-version (eask-read-string "emacs version: (26.1) " nil nil "26.1"))
                        (website (eask-read-string "website: "))
                        (keywords (eask-read-string "keywords: "))
                        (keywords (split-string keywords "[, ]"))
                        (keywords (string-join keywords "\" \""))
                        (content (format
                                  "(package \"%s\"
         \"%s\"
         \"%s\")

(website-url \"%s\")
(keywords \"%s\")

(package-file \"%s\")

(script \"test\" \"echo \\\"Error: no test specified\\\" && exit 1\")
"
                                  package-name version description website keywords
                                  entry-point)))
                   (insert content)

                   (when-let* ((names (mapcar #'car package-archives))
                               (sources (mapcar #'eask--eldev-map-elpa names)))
                     (insert "\n")
                     (dolist (source sources)
                       (insert "(source '" (eask-2str source) ")\n"))))
                 (save-buffer))
               (setq converted t))))
      (if converted "done ✓" "skipped ✗"))
    converted))

(eask-start
  ;; Preparation
  (eask-with-archives "melpa"
    (eask-package-install 'eldev))

  ;; Start Converting
  (require 'eldev)
  (let* ((patterns (eask-args))
         (files (if patterns
                    (eask-expand-file-specs patterns)
                  (directory-files default-directory t "Eldev")))
         (files (cl-remove-if-not (lambda (file)
                                    (string= "Eldev" (file-name-nondirectory file)))
                                  files))
         (converted 0))
    (cond
     ;; Files found, do the action!
     (files
      (eldev--set-up)  ; XXX: Load once!
      (dolist (file files)
        (when (eask--convert-eldev file)
          (cl-incf converted)))
      (eask-msg "")
      (eask-info "(Total of %s Eldev-file%s converted)" converted
                 (eask--sinr converted "" "s")))
     ;; Pattern defined, but no file found!
     (patterns
      (eask-info "(No files match wildcard: %s)"
                 (mapconcat #'identity patterns " ")))
     ;; Default, print help!
     (t
      (eask-info "(No Eldev-files have been converted to Eask)")
      (eask-help "init/eldev")))))

;;; init/eldev.el ends here

;;; init/keg.el --- Initialize Eask from Keg  -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; Commmand use to convert Keg-file to Eask-file
;;
;;   $ eask init --from keg
;;

;;; Code:

(let ((dir (file-name-directory (nth 1 (member "-scriptload" command-line-args)))))
  (load (expand-file-name "_prepare.el"
                          (locate-dominating-file dir "_prepare.el"))
        nil t))

;; Copied from `keg.el'
(defun eask--keg-file-read (path)
  "Return sexp from Keg file (PATH) search from `deafult-directory'.
If no found the Keg file, returns nil."
  (let (sources devs packages lint-disables scripts)
    (when path
      (dolist (elm (read (with-temp-buffer
                           (insert-file-contents path)
                           (format "(%s)" (buffer-string)))))
        (let ((op (car elm))
              (args (cdr elm)))
          (cond
           ((eq 'source op)
            (dolist (elm args) (push elm sources)))
           ((eq 'dev-dependency op)
            (dolist (elm args) (push elm devs)))
           ((eq 'package op)
            (dolist (elm args) (push elm packages)))
           ((eq 'disable-lint op)
            (dolist (elm args) (push elm lint-disables)))
           ((eq 'script op)
            (dolist (elm args) (push elm scripts))))))
      `((sources . ,(nreverse (delete-dups sources)))
        (devs . ,(nreverse (delete-dups devs)))
        (packages . ,(nreverse (delete-dups packages)))
        (disables . ,(nreverse (delete-dups lint-disables)))
        (scripts . ,(nreverse (delete-dups scripts)))))))

(defun eask--convert-keg (filename)
  "Convert Keg FILENAME to Eask."
  (let* ((filename (expand-file-name filename))
         (file (file-name-nondirectory (eask-root-del filename)))
         (new-file (eask-s-replace "Keg" "Eask" file))
         (new-filename (expand-file-name new-file))
         (contents (eask--keg-file-read filename))  ; Read it!
         (converted))
    (eask-with-progress
      (format "Converting file `%s` to `%s`... " file new-file)
      (eask-with-verbosity 'debug
        (cond ((not (string-prefix-p "Keg" file))
               (eask-debug "✗ Invalid Keg filename, the file should start with `Keg`"))
              ((file-exists-p new-filename)
               (eask-debug "✗ The file `%s` already presented" new-file))
              (t
               (with-current-buffer (find-file new-filename)
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

                   (when-let ((scripts (alist-get 'scripts contents)))
                     (dolist (script scripts)
                       (let* ((cmds (cadr script))
                              (_ (pop cmds))
                              (cmds (mapconcat #'identity cmds " ")))
                         (insert "(script \"" (eask-2str (car script))
                                 "\" " (prin1-to-string cmds) ")\n"))))

                   (when-let ((sources (alist-get 'sources contents)))
                     (insert "\n")
                     (dolist (source sources)
                       (insert "(source '" (eask-2str source) ")\n")))

                   (insert "\n")
                   (insert "(depends-on \"emacs\" \"" emacs-version "\")"))
                 (unless (alist-get 'packages contents)
                   (insert "\n"))  ; Make sure end line exists!

                 (when-let ((pkgs (alist-get 'packages contents)))
                   (insert "\n")
                   (dolist (pkg pkgs)
                     (insert "(depends-on \"" (eask-2str (car pkg)) "\")\n")))

                 (when-let ((devs (alist-get 'devs contents)))
                   (insert "\n")
                   (insert "(development\n")
                   (dolist (dev devs)
                     (insert " (depends-on \"" (eask-2str dev) "\")\n"))
                   (insert " )\n"))

                 (save-buffer))
               (setq converted t))))
      (if converted "done ✓" "skipped ✗"))
    converted))

(eask-start
  ;; Preparation
  (eask-with-archives "melpa"
    (eask-package-install 'keg))

  ;; Start Converting
  (require 'keg)
  (let* ((patterns (eask-args))
         (files (if patterns
                    (eask-expand-file-specs patterns)
                  (directory-files default-directory t "Keg")))
         (files (cl-remove-if-not (lambda (file)
                                    (string= "Keg" (file-name-nondirectory file)))
                                  files))
         (converted 0))
    (cond
     ;; Files found, do the action!
     (files
      (dolist (file files)
        (when (eask--convert-keg file)
          (cl-incf converted)))
      (eask-msg "")
      (eask-info "(Total of %s Keg-file%s converted)" converted
                 (eask--sinr converted "" "s")))
     ;; Pattern defined, but no file found!
     (patterns
      (eask-info "(No files match wildcard: %s)"
                 (mapconcat #'identity patterns " ")))
     ;; Default, print help!
     (t
      (eask-info "(No Keg-files have been converted to Eask)")
      (eask-help "init/keg")))))

;;; init/keg.el ends here

;;; core/init.el --- Initialize project to use Eask  -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; Initialize project to use Eask
;;
;;   $ eask init
;;

;;; Code:

(let ((dir (file-name-directory (nth 1 (member "-scriptload" command-line-args)))))
  (load (expand-file-name "_prepare.el"
                          (locate-dominating-file dir "_prepare.el"))
        nil t))

(defun eask-init--check-filename (name)
  "Return non-nil if NAME is a valid Eask-file."
  (when-let* ((name (file-name-nondirectory (directory-file-name name)))
              (prefix (cond ((string-prefix-p "Easkfile" name) "Easkfile")
                            ((string-prefix-p "Eask" name)     "Eask"))))
    (let ((suffix (car (split-string name prefix t))))
      (or (null suffix)
          (string-match-p "^[.][.0-9]*$" suffix)))))

(eask-start
  (let* ((dir (eask-working-directory))
         (files (eask--all-files dir))
         (new-name (expand-file-name "Eask" dir))
         (base-name)
         (invalid-name)
         (continue t))
    (when (and files
               (setq continue
                     (yes-or-no-p (concat "Eask-file already exist,\n\n  "
                                          (mapconcat #'identity files "\n  ")
                                          "\n\nContinue the creation? "))))
      (while (or (file-exists-p new-name) invalid-name)
        (setq new-name (read-file-name
                        (format
                         (concat (if invalid-name
                                     "[?] Invalid filename `%s', "
                                   "[?] Filename `%s' already taken, ")
                                 "try another one: ")
                         (file-name-nondirectory (directory-file-name new-name)))
                        dir nil nil nil
                        #'eask-init--check-filename)
              base-name (file-name-nondirectory (directory-file-name new-name))
              invalid-name (not (eask-init--check-filename base-name)))))
    (when continue
      (eask-println
       "This utility will walk you through creating an Eask file.
It only covers the most common items, and tries to guess sensible defaults.

See `eask init --help` for definitive documentation on these fields
and exactly what they do.

Use `eask install <pkg>` afterwards to install a package and
save it as a dependency in the Eask file.

Press ^C at any time to quit.")
      ;; Starting Eask-file creation!
      (let* ((project-dir (file-name-nondirectory (directory-file-name dir)))
             (project-name (eask-guess-package-name project-dir))
             (package-name (read-string (format "package name: (%s) " project-name) nil nil project-name))
             (version (read-string "version: (1.0.0) " nil nil "1.0.0"))
             (description (read-string "description: "))
             (guess-entry-point (format "%s.el" project-name))
             (entry-point (read-string (format "entry point: (%s) " guess-entry-point)
                                       nil nil guess-entry-point))
             (emacs-version (read-string "emacs version: (26.1) " nil nil "26.1"))
             (website (read-string "website: "))
             (keywords (read-string "keywords: "))
             (keywords (if (string-match-p "," keywords)
                           (split-string keywords ",[ \t\n]*" t "[ ]+")
                         (split-string keywords "[ \t\n]+" t "[ ]+")))
             (keywords (mapconcat (lambda (s) (format "%S" s)) keywords " "))
             (content (format
                       "(package \"%s\"
         \"%s\"
         \"%s\")

(website-url \"%s\")
(keywords %s)

(package-file \"%s\")

(script \"test\" \"echo \\\"Error: no test specified\\\" && exit 1\")

(source \"gnu\")

(depends-on \"emacs\" \"%s\")
"
                       package-name version description website keywords
                       entry-point emacs-version))
             (prompt (format "About to write to %s:\n\n%s\n\nIs this Okay? "
                             new-name content)))
        (when (yes-or-no-p prompt)
          (write-region content nil new-name)
          (find-file new-name))))))

;;; core/init.el ends here

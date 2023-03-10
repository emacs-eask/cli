;;; generate/workflow/github.el --- Generate GitHub Actions workflow yaml file  -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; Command use generate GitHub Actions test yaml file,
;;
;;   $ eask generate workflow github [file]
;;
;;
;;  Positional arguments:
;;
;;    [file]     name of the test file; the default is `test.yml`
;;

;;; Code:

(let ((dir (file-name-directory (nth 1 (member "-scriptload" command-line-args)))))
  (load (expand-file-name "_prepare.el"
                          (locate-dominating-file dir "_prepare.el"))
        nil t))

(eask-start
  (let* ((url "https://raw.githubusercontent.com/emacs-eask/template-generate/master/workflow/github.yml")
         (dir (expand-file-name ".github/workflows/"))
         (basename (or (car (eask-args)) "test.yml"))
         (filename (expand-file-name basename dir))
         (minimum-version (car (cdr (nth 0 eask-depends-on-emacs)))))
    (ignore-errors (make-directory dir t))
    (if (file-exists-p filename)
        (eask-info "The yaml file already exists `%s`" filename)
      (eask-with-progress
        (format "Generating file %s... " filename)
        (eask-with-verbosity 'debug (url-copy-file url filename))
        "done ✓")
      (eask-with-progress
        (format "Configuring file %s... " filename)
        (with-current-buffer (find-file filename)
          (when (search-forward "{ EMACS_VERSION }" nil t)
            (search-backward "{ EMACS_VERSION }" nil t)
            (delete-region (point) (line-end-position))
            (end-of-line)
            (let ((spaces (spaces-string (current-column))))
              (delete-region (line-beginning-position) (line-end-position))
              (when (version<= minimum-version "26.1")
                (insert spaces "- 26.3" "\n"))
              (when (version<= minimum-version "27.1")
                (insert spaces "- 27.2" "\n"))
              (when (version<= minimum-version "28.1")
                (insert spaces "- 28.2" "\n"))
              (when (version<= minimum-version "29.1")
                (insert spaces "- snapshot"))))
          (save-buffer))
        "done ✓")
      (eask-info "✓ Successfully created the yaml file in `%s`" filename))))

;;; generate/workflow/github.el ends here

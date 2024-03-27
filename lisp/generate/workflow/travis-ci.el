;;; generate/workflow/travis-ci.el --- Generate Travis CI workflow yaml file  -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; Command use generate Travis CI test yaml file,
;;
;;   $ eask generate workflow travis-ci [file]
;;
;;
;;  Positionals:
;;
;;    [file]     name of the test file; the default is `.travis.yml`
;;

;;; Code:

(let ((dir (file-name-directory (nth 1 (member "-scriptload" command-line-args)))))
  (load (expand-file-name "_prepare.el"
                          (locate-dominating-file dir "_prepare.el"))
        nil t))

(eask-start
  (let* ((url "https://raw.githubusercontent.com/emacs-eask/template-generate/master/workflow/travis-ci.yml")
         (basename (or (car (eask-args)) ".travis.yml"))
         (filename (expand-file-name basename))
         (minimum-version (car (cdr (nth 0 eask-depends-on-emacs)))))
    (if (file-exists-p filename)
        (eask-info "The yaml file already exists `%s`" filename)
      (eask-with-progress
        (format "  - [1/2] Generating workflow file in %s... " filename)
        (eask-with-verbosity 'debug (url-copy-file url filename))
        "done ✓")
      (eask-with-progress
        (format "  - [2/2] Configuring workflow file in %s... " filename)
        (with-current-buffer (find-file filename)
          (when (search-forward "{ EMACS_VERSION }" nil t)
            (search-backward "{ EMACS_VERSION }" nil t)
            (delete-region (point) (line-end-position))
            (end-of-line)
            (let ((spaces (spaces-string (current-column))))
              (delete-region (line-beginning-position) (line-end-position))
              (when (version<= minimum-version "26.3")
                (insert spaces "- EMACS_CI=emacs-26-3" "\n"))
              (when (version<= minimum-version "27.2")
                (insert spaces "- EMACS_CI=emacs-27-2" "\n"))
              (when (version<= minimum-version "28.2")
                (insert spaces "- EMACS_CI=emacs-28-2" "\n"))
              (when (version<= minimum-version "29.3")
                (insert spaces "- EMACS_CI=emacs-29-3" "\n"))
              (when (version<= minimum-version "30")
                (insert spaces "- EMACS_CI=emacs-snapshot"))))
          (save-buffer))
        "done ✓")
      (eask-msg "")
      (eask-info "✓ Successfully created the yaml file in `%s`" filename))))

;;; generate/workflow/travis-ci.el ends here

;;; generate/workflow/circle-ci.el --- Generate CircleCI workflow yaml file  -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; Command use generate CircleCI test yaml file,
;;
;;   $ eask generate workflow circle-ci
;;
;;
;;  Positional arguments:
;;
;;    [--file]     name of the test file; the default is `config.yml`
;;

;;; Code:

(let ((dir (file-name-directory (nth 1 (member "-scriptload" command-line-args)))))
  (load (expand-file-name "_prepare.el"
                          (locate-dominating-file dir "_prepare.el"))
        nil t))

(defun eask--circle-ci-insert-jobs (version)
  "Insert Circle CI's jobs instruction for specific Emacs' VERSION."
  (insert "  test-ubuntu-emacs-" version ":" "\n")
  (insert "    docker:" "\n")
  (insert "      - image: silex/emacs:" version "-ci" "\n")
  (insert "        entrypoint: bash" "\n")
  (insert "    steps:" "\n")
  (insert "      - setup" "\n")
  (insert "      - test" "\n"))

(eask-start
  (let* ((url "https://raw.githubusercontent.com/emacs-eask/template-generate/master/workflow/circle-ci.yml")
         (dir (expand-file-name ".circleci/"))
         (basename (or (car (eask-args)) "config.yml"))
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
          ;; Config jobs
          (when (search-forward "{ EMACS_VERSION }" nil t)
            (search-backward "{ EMACS_VERSION }" nil t)
            (delete-region (point) (line-end-position))
            (when (version<= minimum-version "26.1")
              (eask--circle-ci-insert-jobs "26"))
            (when (version<= minimum-version "27.1")
              (insert "\n")
              (eask--circle-ci-insert-jobs "27"))
            (when (version<= minimum-version "28.1")
              (insert "\n")
              (eask--circle-ci-insert-jobs "28"))
            (when (version<= minimum-version "29.1")
              (insert "\n")
              (eask--circle-ci-insert-jobs "master")))
          ;; Config matrix
          (when (search-forward "{ MATRIX_JOBS }" nil t)
            (search-backward "{ MATRIX_JOBS }" nil t)
            (delete-region (point) (line-end-position))
            (end-of-line)
            (let ((spaces (spaces-string (current-column))))
              (delete-region (line-beginning-position) (line-end-position))
              (when (version<= minimum-version "26.1")
                (insert spaces "- test-ubuntu-emacs-26" "\n"))
              (when (version<= minimum-version "27.1")
                (insert spaces "- test-ubuntu-emacs-27" "\n"))
              (when (version<= minimum-version "28.1")
                (insert spaces "- test-ubuntu-emacs-28" "\n"))
              (when (version<= minimum-version "29.1")
                (insert spaces "- test-ubuntu-emacs-master"))))
          (save-buffer))
        "done ✓")
      (eask-info "✓ Successfully created the yaml file in `%s`" filename))))

;;; generate/workflow/circle-ci.el ends here

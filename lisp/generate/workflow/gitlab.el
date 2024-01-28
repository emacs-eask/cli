;;; generate/workflow/gitlab.el --- Generate GitLab Runner workflow yaml file  -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; Command use generate GitLab Runner test yaml file,
;;
;;   $ eask generate workflow gitlab  [file]
;;
;;
;;  Positionals:
;;
;;    [file]     name of the test file; the default is `.gitlab-ci.yml`
;;

;;; Code:

(let ((dir (file-name-directory (nth 1 (member "-scriptload" command-line-args)))))
  (load (expand-file-name "_prepare.el"
                          (locate-dominating-file dir "_prepare.el"))
        nil t))

(defun eask--gitlab-insert-jobs (version)
  "Insert GitLab Runner's jobs instruction for specific Emacs' VERSION."
  (insert "test-" version ":" "\n")
  (insert "  image: silex/emacs:" version "-ci" "\n")
  (insert "  script:" "\n")
  (insert "    - eask clean all" "\n")
  (insert "    - eask package" "\n")
  (insert "    - eask install" "\n")
  (insert "    - eask compile" "\n")
  (insert "\n"))

(eask-start
  (let* ((url "https://raw.githubusercontent.com/emacs-eask/template-generate/master/workflow/gitlab.yml")
         (basename (or (car (eask-args)) ".gitlab-ci.yml"))
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
            (when (version<= minimum-version "26.3")
              (eask--gitlab-insert-jobs "26.3"))
            (when (version<= minimum-version "27.2")
              (eask--gitlab-insert-jobs "27.2"))
            (when (version<= minimum-version "28.2")
              (eask--gitlab-insert-jobs "28.2"))
            (when (version<= minimum-version "29.2")
              (eask--gitlab-insert-jobs "29.2"))
            (when (version<= minimum-version "30")
              ;; TODO: snapshot?
              ))
          (delete-trailing-whitespace)
          (save-buffer))
        "done ✓")
      (eask-msg "")
      (eask-info "✓ Successfully created the yaml file in `%s`" filename))))

;;; generate/workflow/gitlab.el ends here

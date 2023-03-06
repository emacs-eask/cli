;;; generate/workflow/github.el --- Generate GitHub Actions workflow `test.yml` file  -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; Command use generate GitHub Actions test yaml file,
;;
;;   $ eask generate workflow github
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
         (filename (expand-file-name basename dir)))
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
            (cond ((version< "28" eask-depends-on-emacs))
                  ;; TODO: ...
                  )
            ))
        "done ✓")
      (eask-info "✓ Successfully created the yaml file in `%s`" filename))))

;;; generate/workflow/github.el ends here

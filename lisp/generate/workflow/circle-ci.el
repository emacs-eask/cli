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
      (eask-info "✓ Successfully created the yaml file in `%s`" filename))))

;;; generate/workflow/circle-ci.el ends here

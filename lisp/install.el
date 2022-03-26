;;; install.el --- Install packages  -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; Command use to install Emacs packages,
;;
;;   $ eask install [names..]
;;
;;
;;  Initialization options:
;;
;;    [names..]     name of the package to install; else we try to install
;;                  package from current directory by calling function
;;                  `package-install-file'
;;
;;  Effective flags:
;;
;;    [-g, --global] [--development, --dev]
;;

;;; Code:

(load-file (expand-file-name
            "_prepare.el"
            (file-name-directory (nth 1 (member "-scriptload" command-line-args)))))

(eask-load "package")

(defun eask--package-tar ()
  "Find a possible package tar file."
  (let* ((name (eask-guess-package-name))
         (version (eask-package-get :version))
         (dist (expand-file-name eask-dist-path))
         (tar (expand-file-name (concat name "-" version ".tar") dist)))
    (message "> %s" tar)
    (and (file-exists-p tar) tar)))

(eask-start
  (eask-pkg-init)
  (if-let ((names (eask-args)))
      ;; If package [name..] are specified, we try to install it
      (dolist (name names) (eask-package-install name))
    ;; Else we try to install package from the working directory
    (mapc #'eask-package-install eask-depends-on)
    (when (eask-dev-p) (mapc #'eask-package-install eask-depends-on-dev))
    ;; Start the normal package installation procedure
    (package-install-file (or (eask--package-tar)
                              eask-package-file
                              (expand-file-name "./")))))

;;; install.el ends here

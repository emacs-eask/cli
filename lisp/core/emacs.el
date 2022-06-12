;;; emacs.el --- Execute emacs with the appropriate environment  -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; Execute emacs with the appropriate environment
;;
;;   $ eask emacs [args..]
;;
;;
;;  Initialization options:
;;
;;    [args..]     arguments feed into Emacs executable
;;

;;; Code:

(load (expand-file-name
       "../_prepare.el"
       (file-name-directory (nth 1 (member "-l" command-line-args))))
      nil t)

;;
;;; Below is copied from `eask-start' macro

(eask--handle-global-options)
(eask--print-env-info)

(setq user-emacs-directory (expand-file-name (concat ".eask/" emacs-version "/"))
      package-user-dir (expand-file-name "elpa" user-emacs-directory)
      user-init-file (locate-user-emacs-file "init.el")
      custom-file (locate-user-emacs-file "custom.el"))

(package-activate-all)
(ignore-errors (make-directory package-user-dir t))
(eask--silent (eask-setup-paths))

;;; emacs.el ends here

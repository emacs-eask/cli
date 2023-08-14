;;; core/emacs.el --- Execute emacs with the appropriate environment  -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; Execute Emacs with the appropriate environment
;;
;;   $ eask emacs [args..]
;;
;;
;;  Positionals:
;;
;;    [args..]     arguments feed into Emacs executable
;;

;;; Code:

(let ((dir (file-name-directory (nth 1 (member "-l" command-line-args)))))
  (load (expand-file-name "_prepare.el"
                          (locate-dominating-file dir "_prepare.el"))
        nil t))

;;
;;; Below is copied from `eask-start' macro

(eask--handle-global-options)

(setq user-emacs-directory (expand-file-name (concat ".eask/" emacs-version "/"))
      package-user-dir (expand-file-name "elpa" user-emacs-directory)
      user-init-file (locate-user-emacs-file "init.el")
      custom-file (locate-user-emacs-file "custom.el"))

(package-activate-all)
(ignore-errors (make-directory package-user-dir t))
(eask--silent (eask-setup-paths))

;;; core/emacs.el ends here

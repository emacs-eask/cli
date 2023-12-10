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
      early-init-file (locate-user-emacs-file "early-init.el")
      eask-dot-emacs-file (locate-user-emacs-file ".emacs")
      user-init-file (locate-user-emacs-file "init.el")
      custom-file (locate-user-emacs-file "custom.el"))

(package-activate-all)
(ignore-errors (make-directory package-user-dir t))
(eask--silent (eask-setup-paths))

;; NOTE: If you modified this execution, make sure you modified the function
;; `eask--load-config' as well!
(let ((inhibit-config (eask-quick-p)))
  (unless inhibit-config
    (when (version<= "27" emacs-version)
      (load early-init-file t t))
    (load eask-dot-emacs-file t t)
    (load user-init-file t t)))

;;; core/emacs.el ends here

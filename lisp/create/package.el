;;; create/package.el --- Create a new elisp project  -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; Create a new elisp project,
;;
;;   $ eask create package [name]
;;
;;
;;  Positionals:
;;
;;    [name]     new project name
;;

;;; Code:

(let ((dir (file-name-directory (nth 1 (member "-scriptload" command-line-args)))))
  (load (expand-file-name "_prepare.el"
                          (locate-dominating-file dir "_prepare.el"))
        nil t))

(defconst eask-create-package--template-name "template-elisp"
  "Holds template project name.")

(defun eask-create-package--replace-s-in-buffer (old new)
  "Replace OLD to NEW in buffer."
  (let ((str (buffer-string)))
    (setq str (eask-s-replace old new str))
    (delete-region (point-min) (point-max))
    (insert str)))

;; XXX: we can't use `user-full-name' in batch-mode. It will always return
;; empty string.
(defun eask-create-package--get-user ()
  "Return user name."
  (string-trim (shell-command-to-string "git config user.name")))

;; XXX: we can't use `user-mail-address' in batch-mode. It will always return
;; empty string.
(defun eask-create-package--get-mail ()
  "Return user email."
  (string-trim (shell-command-to-string "git config user.email")))

(eask-start
  (ignore-errors (delete-directory ".git" t))
  (eask-with-progress
    "Preparing your new elisp project... "
    (let ((template-package-file (expand-file-name (concat eask-create-package--template-name ".el"))))
      (rename-file template-package-file eask-package-file)
      (with-current-buffer (find-file eask-package-file)
        (eask-create-package--replace-s-in-buffer eask-create-package--template-name (eask-package-name))
        (eask-create-package--replace-s-in-buffer "{ SUMMARY }" (eask-package-description))
        (eask-create-package--replace-s-in-buffer "{ YEAR }" (format-time-string "%Y"))
        (eask-create-package--replace-s-in-buffer "{ FULL_NAME }" (eask-create-package--get-user))
        (eask-create-package--replace-s-in-buffer "{ MAIL_ADDR }" (eask-create-package--get-mail))
        (eask-create-package--replace-s-in-buffer "{ WEBSITE_URL }" (or eask-website-url ""))
        (eask-create-package--replace-s-in-buffer "{ VERSION }" (eask-package-version))
        (eask-create-package--replace-s-in-buffer "{ EMACS_VERSION }" (eask-depends-emacs-version))
        (eask-create-package--replace-s-in-buffer "{ KEYWORDS }" (string-join eask-keywords " "))
        (save-buffer)))
    "done ✓")
  (eask-msg "")
  (eask-msg "Congratulations! Your new Elisp project is created in %s" eask-file-root)
  (eask-msg "")
  (eask-msg "  [1] Navigate to %s" eask-file-root)
  (eask-msg "  [2] Try out the command `eask info`")
  (eask-msg "")
  (eask-msg "Visit https://emacs-eask.github.io/ for quickstart guide and full documentation."))

;;; create/package.el ends here

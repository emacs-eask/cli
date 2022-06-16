;;; create.el --- create a new elisp project  -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; Create a new elisp project,
;;
;;   $ eask create [name]
;;
;;
;;  Initialization options:
;;
;;    [name]     new project name
;;

;;; Code:

(load (expand-file-name
       "../_prepare.el"
       (file-name-directory (nth 1 (member "-scriptload" command-line-args))))
      nil t)

(defconst eask--template-project-name "template-elisp"
  "Holds template project name.")

(defun eask--replace-string-in-buffer (old new)
  "Replace OLD to NEW in buffer."
  (let ((str (buffer-string)))
    (setq str (s-replace old new str))
    (delete-region (point-min) (point-max))
    (insert str)))

;; XXX: we can't use `user-full-name' in batch-mode. It will always return
;; empty string.
(defun eask--get-user ()
  "Return user name."
  (string-trim (shell-command-to-string "git config user.name")))

;; XXX: we can't use `user-mail-address' in batch-mode. It will always return
;; empty string.
(defun eask--get-mail ()
  "Return user email."
  (string-trim (shell-command-to-string "git config user.email")))

(eask-start
  (ignore-errors (delete-directory ".git" t))
  (eask-with-progress
    "Preparing your new elisp project... "
    (let ((template-package-file (expand-file-name (concat eask--template-project-name ".el"))))
      (rename-file template-package-file eask-package-file)
      (with-current-buffer (find-file eask-package-file)
        (eask--replace-string-in-buffer eask--template-project-name (eask-package-name))
        (eask--replace-string-in-buffer "{ SUMMARY }" (eask-package-description))
        (eask--replace-string-in-buffer "{ YEAR }" (format-time-string "%Y"))
        (eask--replace-string-in-buffer "{ FULL_NAME }" (eask--get-user))
        (eask--replace-string-in-buffer "{ MAIL_ADDR }" (eask--get-mail))
        (eask--replace-string-in-buffer "{ WEBSITE_URL }" (or eask-website-url ""))
        (eask--replace-string-in-buffer "{ VERSION }" (eask-package-version))
        (eask--replace-string-in-buffer "{ EMACS_VERSION }" (eask-depends-emacs-version))
        (eask--replace-string-in-buffer "{ KEYWORDS }" (string-join eask-keywords " "))
        (save-buffer))
      (with-current-buffer (find-file (expand-file-name "README.md"))
        (eask--replace-string-in-buffer eask--template-project-name (eask-package-name))
        (eask--replace-string-in-buffer "{ SUMMARY }" (eask-package-description))
        (save-buffer)))
    "done ✓")
  (eask-msg "")
  (eask-msg "Congratulations! Your new Elisp project is created in %s" eask-file-root)
  (eask-msg "")
  (eask-msg "  [1] Navigate to %s" eask-file-root)
  (eask-msg "  [2] Try out the command `eask info`")
  (eask-msg "")
  (eask-msg "Visit https://emacs-eask.github.io/ for quickstart guide and full documentation."))

;;; create.el ends here

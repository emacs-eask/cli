;;; create/elpa.el --- Create a new ELPA  -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; Create a new ELPA,
;;
;;   $ eask create elpa [name]
;;
;;
;;  Initialization options:
;;
;;    [name]     new ELPA name
;;

;;; Code:

(load (expand-file-name
       "../_prepare.el"
       (file-name-directory (nth 1 (member "-scriptload" command-line-args))))
      nil t)

(defconst eask--template-elpa-name "template-elpa"
  "Holds template project name.")

(eask-start
  (ignore-errors (delete-directory ".git" t))
  (eask-with-progress
    "Preparing your new ELPA project... "
    (with-current-buffer (find-file (expand-file-name "Eask"))
      (goto-char (point-min))
      (search-forward "(script ")
      (forward-line 1)
      (dolist (gitkeeps (directory-files-recursively eask-file-root ".gitkeep"))
        (ignore-errors (delete-file gitkeeps)))
      ;; --- Start insertion
      (insert "(script \"build\" \"eask exec github-elpa build\")\n")
      (insert "(script \"commit\" \"eask exec github-elpa commit\")\n")
      (insert "(script \"update\" \"eask exec github-elpa update -a \\\"./docs/packages\\\"\")\n")
      ;; --- End insertion
      (save-buffer))
    "done ✓")
  (eask-msg "")
  (eask-msg "Congratulations! Your new ELPA project is created in %s" eask-file-root)
  (eask-msg "")
  (eask-msg "  [1] Navigate to %s" eask-file-root)
  (eask-msg "  [2] Try out the command `eask info`")
  (eask-msg "  [3] See the README.md file to learn to use this project")
  (eask-msg "")
  (eask-msg "Visit https://emacs-eask.github.io/ for quickstart guide and full documentation."))

;;; create/elpa.el ends here

;;; format/elisp-autofmt.el --- Run elisp-autofmt  -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; Commmand use to run `elisp-autofmt' for all files
;;
;;   $ eask format elisp-autofmt [files..]
;;
;;
;;  Positionals:
;;
;;    [files..]     files you want elisp-autofmt to run on
;;

;;; Code:

(let ((dir (file-name-directory (nth 1 (member "-scriptload" command-line-args)))))
  (load (expand-file-name "_prepare.el"
                          (locate-dominating-file dir "_prepare.el"))
        nil t))

;;
;;; Externals

(declare-function elisp-autofmt-buffer "ext:elisp-autofmt.el")

;;
;;; Flags

(advice-add #'eask-allow-error-p :override #'always)

;;
;;; Core

(defconst eask-format-elisp-autofmt--version nil
  "`elisp-autofmt' version.")

(defun eask-format-elisp-autofmt--file (filename)
  "Run elisp-autofmt on FILENAME."
  (let* ((filename (expand-file-name filename))
         (file (eask-root-del filename)))
    (with-current-buffer (find-file filename)
      (elisp-autofmt-buffer)
      (save-buffer)
      (kill-buffer))))

(eask-start
  ;; Preparation
  (eask-with-archives '("gnu" "melpa")
    (eask-package-install 'elisp-autofmt))
  (setq eask-format-elisp-autofmt--version (eask-package--version-string 'elisp-autofmt))

  ;; Start formatting
  (require 'elisp-autofmt)
  (let* ((patterns (eask-args))
         (files (if patterns
                    (eask-expand-file-specs patterns)
                  (eask-package-el-files))))
    (cond
     ;; Files found, do the action!
     (files
      (eask-msg "")
      (eask-msg "Running `%s` formatter (%s)"
                (ansi-green "elisp-autofmt") (ansi-yellow eask-format-elisp-autofmt--version))
      (eask-progress-seq "  - Formatting" files "done! ✓" #'eask-format-elisp-autofmt--file)
      (eask-msg "")
      (eask-info "(Total of %s file%s %s formatted)" (length files)
                 (eask--sinr files "" "s")
                 (eask--sinr files "has" "have")))
     ;; Pattern defined, but no file found!
     (patterns
      (eask-info "(No files match wildcard: %s)"
                 (mapconcat #'identity patterns " ")))
     ;; Default, print help!
     (t
      (eask-info "(No files have been formatted)")
      (eask-help "format/elisp-autofmt")))))

;;; format/elisp-autofmt.el ends here

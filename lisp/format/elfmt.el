;;; format/elfmt.el --- Run elfmt  -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; Commmand use to run `elfmt' for all files
;;
;;   $ eask format elfmt [files..]
;;
;;
;;  Positionals:
;;
;;    [files..]     files you want elfmt to run on
;;

;;; Code:

(let ((dir (file-name-directory (nth 1 (member "-scriptload" command-line-args)))))
  (load (expand-file-name "_prepare.el"
                          (locate-dominating-file dir "_prepare.el"))
        nil t))

;;
;;; Externals

(declare-function elfmt-buffer "ext:elisp-autofmt.el")

;;
;;; Flags

(advice-add #'eask-allow-error-p :override #'always)

;;
;;; Core

(defconst eask-format-elfmt--version nil
  "`elfmt' version.")

(defun eask-format-elfmt--file (filename)
  "Run elfmt on FILENAME."
  (let* ((filename (expand-file-name filename))
         (file (eask-root-del filename)))
    (with-current-buffer (find-file filename)
      (elfmt-buffer)
      (save-buffer)
      (kill-buffer))))

(eask-start
  ;; Preparation
  (eask-with-archives '("gnu" "melpa" "jcs-elpa")
    (eask-package-install 'elfmt))
  (setq eask-format-elfmt--version (eask-package--version-string 'elfmt))

  ;; Start formatting
  (require 'elfmt)
  (let* ((patterns (eask-args))
         (files (if patterns
                    (eask-expand-file-specs patterns)
                  (eask-package-el-files))))
    (cond
     ;; Files found, do the action!
     (files
      (eask-msg "")
      (eask-msg "Running `%s` formatter (%s)"
                (ansi-green "elfmt") (ansi-yellow eask-format-elfmt--version))
      (eask-progress-seq "  - Formatting" files "done! ✓" #'eask-format-elfmt--file)
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
      (eask-help "format/elfmt")))))

;;; format/elfmt.el ends here

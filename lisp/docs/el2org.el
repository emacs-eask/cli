;;; docs/el2org.el --- Build documentation with el2org  -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; Build documentation for your Elisp project,
;;
;;   $ eask docs [names..]
;;
;;
;;  Positionals:
;;
;;    [names..]         specify files to scan
;;
;;  Action options:
;;
;;    [destination]     optional output destination
;;

;;; Code:

(let ((dir (file-name-directory (nth 1 (member "-scriptload" command-line-args)))))
  (load (expand-file-name "_prepare.el"
                          (locate-dominating-file dir "_prepare.el"))
        nil t))

;;
;;; Externals

(require 'el2org nil t)

;;
;;; Core

(defun eask-docs--to-html (el-file)
  "Generate html file from EL-FILE."
  (interactive)
  (let* ((filename (file-name-nondirectory el-file))
         (html-file (expand-file-name (concat (file-name-sans-extension filename)
                                              ".html")
                                      eask-docs-path)))
    (eask-with-verbosity 'debug
      (el2org-generate-file el-file nil 'html html-file t))))

(eask-start
  ;; Preparation
  (eask-archive-install-packages '("gnu" "melpa")
                                 'el2org)

  ;; Start building...
  (require 'el2org)
  (let* ((patterns (eask-args))
         (files (if patterns (eask-expand-file-specs patterns)
                  (eask-package-el-files)))
         (eask-docs-path (or (eask-dest) eask-docs-path)))
    (cond
     ;; Files found, do the action!
     (files
      (eask-debug "Destination path in %s" eask-docs-path)
      (ignore-errors (make-directory eask-docs-path t))

      (eask-progress-seq
       "  - Processing" files "done! ✓" #'eask-docs--to-html)
      (eask-msg "")
      (eask-info "Done. (Converted %s file%s)" (length files)
                 (eask--sinr files "" "s")))
     ;; Pattern defined, but no file found!
     (patterns
      (eask-info "(No files match wildcard: %s)"
                 (mapconcat #'identity patterns " ")))
     ;; Default, print help!
     (t
      (eask-info "(No elisp source can be read)")
      (eask-help "core/docs")))))

;;; docs/el2org.el ends here

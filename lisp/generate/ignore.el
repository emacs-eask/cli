;;; generate/ignore.el --- Generate ignore file  -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; Command use generate ignore file,
;;
;;   $ eask generate ignore <name>
;;
;;
;;  Positionals:
;;
;;    <name>       Name of the ignore template
;;
;;  Optional arguments:
;;
;;    --output     Output result to a file; the default is `.gitignore`
;;

;;; Code:

(let ((dir (file-name-directory (nth 1 (member "-scriptload" command-line-args)))))
  (load (expand-file-name "_prepare.el"
                          (locate-dominating-file dir "_prepare.el"))
        nil t))

(defun eask--print-ignore-menu ()
  "Print all available ignore."
  (eask-msg "available via `eask generate ignore`")
  (eask-msg "")
  (let ((names (gitignore-templates-names)))
    (dolist (data names)
      (eask-msg "  %s" data))
    (eask-msg "")
    (eask-info "(Total of %s available ignore file%s)" (length names)
               (eask--sinr names "" "s"))))

(eask-start
  ;; Preparation
  (eask-with-archives "melpa"
    (eask-package-install 'gitignore-templates))

  ;; Start the task
  (require 'gitignore-templates)
  (let* ((name (car (eask-args)))  ; type of the ignore
         (basename (or (eask-output) ".gitignore"))
         (filename (expand-file-name basename)))
    (eask-msg "")
    (cond ((file-exists-p filename)
           (eask-info "(The ignore file already exists `%s`)" filename))
          ((not (member name (gitignore-templates-names)))
           (eask-info "(Invalid ignore type: `%s`)" name)
           (eask--print-ignore-menu))
          (t
           (eask-with-progress
             (format "Generating ignore file in %s... " filename)
             (with-current-buffer (find-file filename)
               (gitignore-templates-insert name)
               (save-buffer))
             "done ✓")))))

;;; generate/ignore.el ends here

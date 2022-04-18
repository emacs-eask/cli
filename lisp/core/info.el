;;; info.el --- Display information about the current package  -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; Display information about the current package
;;
;;   $ eask info
;;

;;; Code:

(load (expand-file-name
       "../_prepare.el"
       (file-name-directory (nth 1 (member "-scriptload" command-line-args))))
      nil t)

(defvar eask--max-offset 0)

(defun eask--print-deps (title dependencies)
  "Print dependencies."
  (when dependencies
    (eask-msg "")
    (eask-msg title)
    (let* ((names (mapcar #'car dependencies))
           (offset (eask-seq-str-max names)))
      (setq eask--max-offset (max offset eask--max-offset)
            offset (format "%s" eask--max-offset))
      (dolist (dep dependencies)
        (let* ((target-version (cdr dep))
               (target-version (if (= (length target-version) 1)
                                   (nth 0 target-version)
                                 "specified")))
          (eask-msg (concat "  %-" offset "s (%s)") (car dep) target-version)
          (eask-debug "    Recipe: %s" (car dep)))))))

(defun eask--package-desc-url ()
  "Return url from package descriptor."
  (when-let ((extras (package-desc-extras eask-package-desc)))
    (cdr (assoc :url extras))))

(eask-start
  (if eask-package
      (progn
        (eask-msg "")
        (eask-msg "%s (%s) | deps: %s | devDeps: %s"
                  (ansi-green (eask-package-name))
                  (ansi-yellow (eask-package-version))
                  (ansi-cyan (length eask-depends-on))
                  (ansi-cyan (length eask-depends-on-dev)))
        (eask-msg (eask-package-description))
        (eask-msg (ansi-cyan (eask--package-desc-url)))
        (when eask-package-desc
          (when-let ((keywords (package-desc--keywords eask-package-desc)))
            (eask-msg "")
            (eask-msg "keywords: %s" (mapconcat #'identity keywords ", "))))
        (eask-msg "")
        (when eask-package-file
          (eask-msg "entry: %s" (eask-root-del eask-package-file)))
        (eask-msg "kind: %s" (if (eask-package-multi-p) "tar" "single"))
        (eask--print-deps "dependencies:" eask-depends-on)
        (eask--print-deps "devDependencies:" eask-depends-on-dev))
    (eask-info "(Eask file has no package information)")
    (eask-help 'info)))

;;; info.el ends here

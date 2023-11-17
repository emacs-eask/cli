;;; core/info.el --- Display information about the current package  -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; Display information about the current package
;;
;;   $ eask info
;;

;;; Code:

(let ((dir (file-name-directory (nth 1 (member "-scriptload" command-line-args)))))
  (load (expand-file-name "_prepare.el"
                          (locate-dominating-file dir "_prepare.el"))
        nil t))

(defvar eask--max-offset 0)

(defun eask--print-deps (title dependencies)
  "Print DEPENDENCIES with TITLE identifier."
  (when dependencies
    (eask-println "")
    (eask-println title)
    (let* ((names (mapcar #'car dependencies))
           (offset (eask-seq-str-max names)))
      (setq eask--max-offset (max offset eask--max-offset)
            offset (eask-2str eask--max-offset))
      (dolist (dep dependencies)
        (let* ((target-version (cdr dep))
               (target-version (if (= (length target-version) 1)
                                   (nth 0 target-version)
                                 "specified")))
          (eask-println (concat "  %-" offset "s (%s)") (car dep) target-version)
          (eask-debug "    Recipe: %s" (car dep)))))))

(eask-start
  (if eask-package
      (progn
        (eask-println "%s (%s) | deps: %s | devDeps: %s"
                  (ansi-green (eask-package-name))
                  (ansi-yellow (eask-package-version))
                  (ansi-cyan (eask-2str (length eask-depends-on)))
                  (ansi-cyan (eask-2str (length eask-depends-on-dev))))
        (unless (string-empty-p (eask-package-description))
          (eask-println (eask-package-description)))
        (when-let* ((url (or (eask-package-desc-url) eask-website-url))
                    ((not (string-empty-p url))))
          (eask-println (ansi-cyan url)))
        (when-let ((keywords (or (eask-package-desc-keywords) eask-keywords)))
          (eask-println "")
          (eask-println "keywords: %s" (string-join keywords ", ")))
        (eask-println "")
        (when eask-package-file
          (eask-println "entry: %s" (eask-root-del eask-package-file)))
        (eask-println "kind: %s" (if (eask-package-multi-p) "tar" "single"))
        (eask-println "")
        (eask-println "dist")
        (eask-println ".total-files: %s" (length (eask-package-files)))
        (eask-println ".unpacked-size: %s" (eask-unpacked-size))
        (eask--print-deps "dependencies:" eask-depends-on)
        (eask--print-deps "devDependencies:" eask-depends-on-dev))
    (eask-info "(Eask file has no package information)")
    (eask-help "core/info")))

;;; core/info.el ends here

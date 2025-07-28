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

(defvar eask-info--max-offset 0)

(defun eask-info--print-deps (title dependencies)
  "Print DEPENDENCIES with TITLE identifier."
  (when dependencies
    (eask-println "")
    (eask-println title)
    (let* ((names (mapcar #'car dependencies))
           (offset (eask-seq-str-max names)))
      (setq eask-info--max-offset (max offset eask-info--max-offset)
            offset (eask-2str eask-info--max-offset))
      (dolist (dep dependencies)
        (let* ((target-version (cdr dep))
               (target-version (cond ((memq :file dep) "file")
                                     ((memq :vc dep)   "vc")
                                     ((memq :try dep)  "try")
                                     ((= (length target-version) 1)
                                      (or (nth 0 target-version)  ; verison number
                                          "archive"))
                                     (t                "recipe"))))
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
          (eask-println (ansi-blue url)))
        (when-let* ((keywords (or (eask-package-desc-keywords) eask-keywords))
                    (keywords (mapcar (lambda (keyword) (ansi-cyan keyword)) keywords)))
          (eask-println "")
          (eask-println "keywords: %s" (string-join keywords ", ")))
        (eask-println "")
        (when eask-package-file
          (eask-println "entry: %s" (eask-root-del eask-package-file)))
        (eask-println "kind: %s" (ansi-cyan (if (eask-package-multi-p) "tar" "single")))
        (eask-println "")
        (eask-println "dist")
        (eask-println ".total-files: %s" (ansi-magenta (eask-2str (length (eask-package-files)))))
        (eask-println ".unpacked-size: %s" (ansi-magenta (eask-unpacked-size)))
        (eask-info--print-deps "dependencies:" eask-depends-on)
        (eask-info--print-deps "devDependencies:" eask-depends-on-dev))
    (eask-info "(Eask file has no package information)")
    (eask-help "core/info")))

;;; core/info.el ends here

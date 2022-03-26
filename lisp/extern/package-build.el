;;; package-build.el --- External module `package-build'  -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(require 'package-build nil t)

(defun package-build--create-tar (name version directory)
  "Create a tar file containing the contents of VERSION of package NAME."
  (let ((tar (expand-file-name (concat name "-" version ".tar")
                               package-build-archive-dir))
        (dir (concat name "-" version)))
    ;; XXX https://github.com/melpa/package-build/pull/34
    ;; (when (eq system-type 'windows-nt)
    ;;   (setq tar (replace-regexp-in-string "^\\([a-z]\\):" "/\\1" tar)))
    (let ((default-directory directory))
      (process-file package-build-tar-executable nil
                    (get-buffer-create "*package-build-checkout*") nil
                    "-cvf" tar
                    "--exclude=.git"
                    "--exclude=.hg"
                    dir))
    (when (and package-build-verbose noninteractive)
      (message "Created %s containing:" (file-name-nondirectory tar))
      (dolist (line (sort (process-lines package-build-tar-executable
                                         "--list" "--file" tar)
                          #'string<))
        (message "  %s" line)))))

(defun package-build-expand-file-specs (dir specs &optional subdir allow-empty)
  "In DIR, expand SPECS, optionally under SUBDIR.
The result is a list of (SOURCE . DEST), where SOURCE is a source
file path and DEST is the relative path to which it should be copied.

If the resulting list is empty, an error will be reported.  Pass t
for ALLOW-EMPTY to prevent this error."
  (let ((default-directory dir)
        (prefix (if subdir (format "%s/" subdir) ""))
        (lst))
    (dolist (entry specs)
      (setq lst
            (if (consp entry)
                (if (eq :exclude (car entry))
                    (cl-nset-difference lst
                                        (package-build-expand-file-specs
                                         dir (cdr entry) nil t)
                                        :key #'car
                                        :test #'equal)
                  (nconc lst
                         (package-build-expand-file-specs
                          dir
                          (cdr entry)
                          (concat prefix (car entry))
                          t)))
              (nconc
               lst (mapcar (lambda (f)
                             (cons f
                                   (concat prefix
                                           (replace-regexp-in-string
                                            "\\.el\\.in\\'"
                                            ".el"
                                            (file-name-nondirectory f)))))
                           (file-expand-wildcards entry))))))
    (when (and (null lst) (not allow-empty))
      (error "No matching file(s) found in %s: %s" dir specs))
    lst))

;;; package-build.el ends here

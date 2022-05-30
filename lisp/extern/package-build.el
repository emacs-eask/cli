;;; package-build.el --- External module `package-build'  -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(require 'package-build nil t)

(defun package-build--create-tar (name version directory mtime)
  "Create a tar file containing the contents of VERSION of package NAME.
DIRECTORY is a temporary directory that contains the directory
that is put in the tarball.  MTIME is used as the modification
time of all files, making the tarball reproducible."
  (let ((tar (expand-file-name (concat name "-" version ".tar")
                               package-build-archive-dir))
        (dir (concat name "-" version)))
    ;; XXX: https://github.com/melpa/package-build/pull/34
    ;;
    ;; We definitely need to remove these two lines, or else it won't able to
    ;; build on Windows.
    ;;
    ;; (when (eq system-type 'windows-nt)
    ;;   (setq tar (replace-regexp-in-string "^\\([a-z]\\):" "/\\1" tar)))
    (let ((default-directory directory))
      (process-file
       package-build-tar-executable nil
       (get-buffer-create "*package-build-checkout*") nil
       "-cf" tar dir
       ;; Arguments that are need to strip metadata that
       ;; prevent a reproducable tarball as described at
       ;; https://reproducible-builds.org/docs/archives.
       "--sort=name"
       (format "--mtime=@%d" mtime)
       "--owner=0" "--group=0" "--numeric-owner"
       "--pax-option=exthdr.name=%d/PaxHeaders/%f,delete=atime,delete=ctime"))
    (when (and package-build-verbose noninteractive)
      (message "Created %s containing:" (file-name-nondirectory tar))
      (dolist (line (sort (process-lines package-build-tar-executable
                                         "--list" "--file" tar)
                          #'string<))
        (message "  %s" line)))))

(defun package-build--build-multi-file-package (rcp version commit files source-dir)
  (let* ((name (oref rcp name))
         (tmp-dir (file-name-as-directory (make-temp-file name t))))
    (unwind-protect
        (let* ((target (expand-file-name (concat name "-" version) tmp-dir))
               (desc (let ((default-directory source-dir))
                       (or (package-build--desc-from-package
                            name version commit files)
                           (package-build--desc-from-library
                            name version commit files 'tar)
                           (error "%s[-pkg].el matching package name is missing"
                                  name))))
               ;; XXX: https://github.com/emacs-eask/eask/issues/29
               ;;
               ;; Building directory recipe, ignore timestamp.
               ;;
               ;;(mtime (package-build--get-timestamp rcp commit))
               (mtime 20220531.0000))
          (package-build--copy-package-files files source-dir target)
          (package-build--write-pkg-file desc target)
          (package-build--generate-info-files files source-dir target)
          (package-build--create-tar name version tmp-dir mtime)
          (package-build--write-pkg-readme name files source-dir)
          (package-build--write-archive-entry desc))
      (delete-directory tmp-dir t nil))))

;;
;; NOTE: This is brought in cuz it's very useful, but we don't want to bring the
;; whole `package-build' package unless it's needed.
;;
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

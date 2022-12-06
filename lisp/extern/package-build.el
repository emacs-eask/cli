;;; extern/package-build.el --- External module `package-build'  -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(require 'package-build nil t)

(defun package-build--create-tar (rcp directory)
  "Create a tar file containing the package version specified by RCP.
DIRECTORY is a temporary directory that contains the directory
that is put in the tarball."
  (let* ((name (oref rcp name))
         (version (oref rcp version))
         (time (oref rcp time))
         (tar (expand-file-name (concat name "-" version ".tar")
                                package-build-archive-dir))
         (dir (concat name "-" version)))
    ;; XXX: https://github.com/melpa/package-build/pull/34
    ;;
    ;; We definitely need to remove these two lines, or else it won't able to
    ;; build on Windows.
    ;;
    (when (and (eq system-type 'windows-nt)
               (not (string-prefix-p "bsdtar" eask-tar-version-string)))
      (setq tar (replace-regexp-in-string "^\\([a-z]\\):" "/\\1" tar)))
    (let ((default-directory directory))
      (process-file
       package-build-tar-executable nil
       (get-buffer-create "*package-build-checkout*") nil
       "-cf" tar dir
       ;; Arguments that are need to strip metadata that
       ;; prevent a reproducable tarball as described at
       ;; https://reproducible-builds.org/docs/archives.
       "--sort=name"
       (format "--mtime=@%d" time)
       "--owner=0" "--group=0" "--numeric-owner"
       "--pax-option=exthdr.name=%d/PaxHeaders/%f,delete=atime,delete=ctime"))
    (when (and package-build-verbose noninteractive)
      (message "Created %s containing:" (file-name-nondirectory tar))
      (dolist (line (sort (process-lines package-build-tar-executable
                                         "--list" "--file" tar)
                          #'string<))
        (message "  %s" line)))))

;;
;; NOTE: Following code are brought in cuz it's very useful, but we don't want
;; to bring the whole `package-build' package unless it's needed.
;;

(defconst package-build-default-files-spec
  '("*.el" "lisp/*.el"
    "dir" "*.info" "*.texi" "*.texinfo"
    "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo"
    "docs/dir" "docs/*.info" "docs/*.texi" "docs/*.texinfo"
    (:exclude
     ".dir-locals.el" "lisp/.dir-locals.el"
     "test.el" "tests.el" "*-test.el" "*-tests.el"
     "lisp/test.el" "lisp/tests.el" "lisp/*-test.el" "lisp/*-tests.el"))
  "Default value for :files attribute in recipes.")

(defun package-build-expand-files-spec (rcp &optional assert repo spec)
  "No documentation."
  (let ((default-directory (or repo (package-recipe--working-tree rcp)))
        (spec (or spec (oref rcp files))))
    (when (eq (car spec) :defaults)
      (setq spec (append package-build-default-files-spec (cdr spec))))
    (let ((files (package-build--expand-files-spec-1
                  (or spec package-build-default-files-spec))))
      (when assert
        (when (and rcp spec
                   (equal files (package-build--expand-files-spec-1
                                 package-build-default-files-spec)))
          (message "Warning: %s :files spec is equivalent to the default"
                   (oref rcp name)))
        (unless files
          (error "No matching file(s) found in %s using %s"
                 default-directory (or spec "default spec"))))
      files)))

(defun package-build--expand-files-spec-1 (spec &optional subdir)
  (let ((files nil))
    (dolist (entry spec)
      (setq files
            (cond
             ((stringp entry)
              (nconc files
                     (mapcar (lambda (f)
                               (cons f
                                     (concat subdir
                                             (replace-regexp-in-string
                                              "\\.el\\.in\\'"  ".el"
                                              (file-name-nondirectory f)))))
                             (file-expand-wildcards entry))))
             ((eq (car entry) :exclude)
              (cl-nset-difference
               files
               (package-build--expand-files-spec-1 (cdr entry))
               :key #'car :test #'equal))
             (t
              (nconc files
                     (package-build--expand-files-spec-1
                      (cdr entry)
                      (concat subdir (car entry) "/")))))))
    files))

;;; extern/package-build.el ends here

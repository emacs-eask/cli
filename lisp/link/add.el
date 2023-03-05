;;; link/add.el --- Link a local package  -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; Commmand use to link a local package
;;
;;   $ eask link add [name] [path]
;;
;;
;;  Initialization options:
;;
;;    [name]     name of the link
;;    [path]     location (target package) where you want to link
;;

;;; Code:

(load (expand-file-name
       "../_prepare.el"
       (file-name-directory (nth 1 (member "-scriptload" command-line-args))))
      nil t)

(eask-load "link/list")
(eask-load "link/delete")
(eask-load "core/install")

(defun eask--package-desc-reqs (desc)
  "Return a list of requirements from package DESC."
  (cl-remove-if (lambda (name) (string= name "emacs"))
                (mapcar #'car (package-desc-reqs desc))))

(defvar eask--link-package-name    nil "Used to form package name.")
(defvar eask--link-package-version nil "Used to form package name.")

(defun eask--create-link (name source)
  "Add link with NAME to PATH."
  (let* ((dir-name (format "%s-%s" eask--link-package-name eask--link-package-version))
         (link-path (expand-file-name dir-name package-user-dir)))
    (when (file-exists-p link-path)
      (eask-with-progress
        (ansi-yellow "!! The link already present; override the existing link... ")
        (eask--delete-symlink link-path)
        (ansi-yellow "done !!")))
    (make-symbolic-link source link-path)
    (eask-msg "")
    (eask-info "✓ Created link from `%s` to `%s`" source (eask-f-filename link-path))))

(eask-start
  (let* ((names (eask-args))
         (name (nth 0 names))
         (path (nth 1 names))
         (source (expand-file-name path)))
    (cond
     ;; Wrong arguments number.
     ((<= 3 (length names))
      (eask-info "✗ This command only accepts maximum of 2 arguments: %s"
                 (mapconcat #'identity names " ")))
     ;; Source path not found!
     ((not (file-directory-p source))
      (eask-info "✗ Can't create link %s to non-existing path: %s" name source))
     ;; Create the link
     (t
      (let ((links (eask--links))
            (pkg-el (expand-file-name (package--description-file name) source))
            (pkg-eask (car (eask--all-files source)))
            (pkg-desc))
        (cond
         ((ignore-errors (file-exists-p pkg-el))
          (with-temp-buffer
            (insert-file-contents pkg-el)
            (setq pkg-desc (ignore-errors
                             (if pkg-el
                                 (package--read-pkg-desc 'dir)
                               (package-buffer-info)))))
          (setq eask--link-package-name (package-desc-name pkg-desc)
                eask--link-package-version (package-version-join
                                            (package-desc-version pkg-desc)))
          ;; XXX: Install dependencies for linked package
          (eask--install-packages (eask--package-desc-reqs pkg-desc))
          (eask--create-link name source)
          (when (and (zerop (length links))         ; if no link previously,
                     (= 1 (length (eask--links))))  ; and first link created!
            (eask-help "link/add/success/pkg")))
         ((ignore-errors (file-exists-p pkg-eask))
          (eask--save-load-eask-file pkg-eask
              (progn
                (setq eask--link-package-name (eask-package-name)
                      eask--link-package-version (eask-package-version))
                ;; XXX: Install dependencies for linked package
                (eask-install-dependencies)
                ;; Help generates necessary files!
                (let* ((default-directory source)  ; this to make command work!
                       (pkg-file       (expand-file-name (concat name "-pkg.el")))
                       (autoloads-file (expand-file-name (concat name "-autoloads.el")))
                       (pkg-file-presented       (file-exists-p pkg-file))
                       (autoloads-file-presented (file-exists-p autoloads-file)))
                  (eask-msg "")
                  (eask-with-progress
                    (format "Generating %s file... " autoloads-file)
                    (unless autoloads-file-presented
                      (eask-with-verbosity 'debug (eask-call "core/autoloads")))
                    (if autoloads-file-presented "already presented ✗" "done ✓"))
                  (eask-with-progress
                    (format "Generating %s file... " pkg-file)
                    (unless pkg-file-presented
                      (eask-with-verbosity 'debug (eask-call "core/pkg-file")))
                    (if pkg-file-presented "already presented ✗" "done ✓")))
                (eask--create-link name source)
                (when (and (zerop (length links))         ; if no link previously,
                           (= 1 (length (eask--links))))  ; and first link created!
                  (eask-help "link/add/success/eask")))
            (eask-error "✗ Error loading Eask-file: %s" pkg-eask)))
         (t
          (eask-info "✗ (Missing `%s-pkg.el` file in your source folder)" name)
          (eask-help "link/add/error"))))))))

;;; link/add.el ends here

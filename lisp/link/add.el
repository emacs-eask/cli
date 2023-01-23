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

(defvar eask--link-package-name)
(defvar eask--link-package-version)

(defun eask--create-link (name source)
  "Add link with NAME to PATH."
  (let* ((dir-name (format "%s-%s" eask--link-package-name eask--link-package-version))
         (link-path (expand-file-name dir-name package-user-dir)))
    (eask--delete-symlink link-path)
    (make-symbolic-link source link-path)
    (eask-msg "")
    (eask-info "✓ Created link from %s to %s" source (eask-f-filename link-path))))

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
     ((not (directory-name-p source))
      (eask-info "✗ Can't create link %s to non-existing path: %s" name source))
     ;; Create the link
     (t
      (let ((pkg-el   (package--description-file source))
            (pkg-eask (car (eask--all-files source)))
            (pkg-desc))
        (cond ((file-exists-p pkg-el)
               (with-temp-buffer
                 (insert-file-contents pkg-el)
                 (setq pkg-desc (ignore-errors
                                  (if pkg-el
                                      (package--read-pkg-desc 'dir)
                                    (package-buffer-info))))))
              ((file-exists-p pkg-eask)
               (eask--save-load-eask-file pkg-eask
                   (progn
                     (setq eask--link-package-name (eask-package-name)
                           eask--link-package-version (eask-package-version))
                     (let ((default-directory source))
                       (eask-call "core/pkg-file")
                       (eask-call "core/autoloads")))
                 (eask-info "✗ Error loading Eask-file: %s" pkg-eask)))
              (t
               (eask-info "✗ Link source %s doesn't have an Eask or %s-pkg.el file"
                          source name))))
      (eask--create-link name source)))))

;;; link/add.el ends here

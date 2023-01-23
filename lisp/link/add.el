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

(defvar eask--link-package-name)
(defvar eask--link-package-version)

(defun eask--create-link (name source)
  "Add link with NAME to PATH."
  (let* ((dir-name (format "%s-%s" eask--link-package-name eask--link-package-version))
         (link-path (expand-file-name dir-name package-user-dir)))
    (f-symlink source link-path)
    (eask-msg "Created link from %s to %s" source to link-path)))

(eask-start
  (eask-with-archives "melpa"
    (eask-package-install 'f))
  (require 'f)
  (let* ((names (eask-args))
         (name (nth 0 names))
         (path (nth 1 names))
         (source (expand-file-name path))
         (links (eask--links))
         (link (assoc name links)))
    (cond
     ;; Wrong arguments number.
     ((<= 3 (length names))
      (eask-info "This command only accepts maximum of 2 arguments: %s"
                 (mapconcat #'identity names " ")))
     ;; Link already exists
     (link
      (eask-info "The link %s already exists, and has the value of %s" (car link) (cdr link))
      (eask-info "Please remove it before you link to a new location!"))
     ;; Source path not found!
     ((not (directory-name-p source))
      (eask-info "Can't create link %s to non-existing path: %s" name source))
     ;; Load Eask-file and pkg-file
     ((let ((pkg-el   (package--description-file source))
            (pkg-eask (eask--all-files source))
            (pkg-desc))
        (cond ((file-exists-p pkg-el)
               (with-temp-buffer
                 (insert-file-contents pkg-el)
                 (setq pkg-desc (ignore-errors
                                  (if pkg-el
                                      (package--read-pkg-desc 'dir)
                                    (package-buffer-info))))))
              ((file-exists-p pkg-eask)

               )
              (t
               (eask-info "Link source %s doesn't have an Eask or %s-pkg.el file"
                          source name)))
        pkg-desc))
     ;; Create the link
     (t
      (eask--create-link name source)))))

;;; link/add.el ends here

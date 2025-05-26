;;; core/install-file.el --- Install packages from files, .tar files, or directories  -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; Command use to install packages from files, .tar files, or directories
;;
;;   $ eask install-file [files..]
;;
;;
;;  Positionals:
;;
;;    [files..]     files to install as packages; it will install through the
;;                  function `package-install-file'
;;

;;; Code:

(let ((dir (file-name-directory (nth 1 (member "-scriptload" command-line-args)))))
  (load (expand-file-name "_prepare.el"
                          (locate-dominating-file dir "_prepare.el"))
        nil t))

(eask-load "core/install")

(defun eask-install-file--get-package-name (path)
  "Get the package name from PATH, which is a file, directory or archive."
  (let ((path (expand-file-name path)))
    (cond
     ((not (file-exists-p path))
      (eask-error "✗ File does not exist in `%s`" path))
     ;; TAR file
     ((string-match-p "[.]+tar[.]*" path)
      ;; Note this can throw strange errors if
      ;;
      ;; - there is no -pkg.el in the tar file
      ;; - the tar file was built in a folder with a different name
      ;;
      ;; TAR files created with eask package are fine.
      (require 'tar-mode)
      (let ((pkg-desc (with-temp-buffer
                        (insert-file-contents-literally path)
                        (tar-mode)
                        (ignore-errors (package-tar-file-info)))))
        (unless pkg-desc
          ;; `package-dir-info' will return nil if there is no `-pkg.el'
          ;; and no `.el' files at path
          (eask-error "✗ No package in `%s`" path))
        (package-desc-name pkg-desc)))
     ;; .el file or directory
     (t
      ;; Note `package-dir-info' doesn't work outside of dired mode!
      (let ((pkg-desc (with-temp-buffer
                        (dired path)
                        ;; After Emacs 31, the function `package-dir-info'
                        ;; will respect the marked files.
                        ;;
                        ;; See https://debbugs.gnu.org/cgi/bugreport.cgi?bug=78521#17
                        ;;(dired-mark-files-in-region (point-min) (point-max))
                        (eask--unsilent
                          (message "? %s" (dired-get-marked-files))
                          (ignore-errors (package-dir-info))))))
        (unless pkg-desc
          ;; `package-dir-info' will return nil if there is no `-pkg.el'
          ;; and no `.el' files at path
          (eask-error "✗ No package in `%s`" path))
        (package-desc-name pkg-desc))))))

(defun eask-install-file--packages (files)
  "The file install packages with FILES."
  (let* ((deps (mapcar (lambda (file)
                         (list (eask-install-file--get-package-name file) file))
                       files))
         (names (mapcar #'car deps))
         (len (length deps))
         (s (eask--sinr len "" "s"))
         (not-installed (eask-install--not-installed names))
         (installed (length not-installed))
         (skipped (- len installed)))
    (eask-log "Installing %s specified file package%s..." len s)
    (eask-msg "")
    (eask--package-mapc (lambda (dep &rest _)
                          (apply #'eask-package-install-file dep))
                        deps)
    (eask-msg "")
    (eask-info "(Total of %s file package%s installed, %s skipped)"
               installed s skipped)))

(eask-start
  (eask-pkg-init)
  (if-let* ((files (eask-args)))
      ;; If package [files..] are specified, we try to install it
      (eask-install-file--packages files)
    ;; Otherwise, report error.
    (eask-info "(No file packages have been installed)")
    (eask-help "core/install-file")))

;;; core/install-file.el ends here

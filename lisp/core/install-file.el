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

(defun eask-install-file--guess-name (file)
  "Guess the package name of the install FILE."
  (file-name-sans-extension (file-name-nondirectory (directory-file-name file))))

(defun eask-install-file--packages (files)
  "The file install packages with FILES."
  (let* ((deps (mapcar (lambda (file)
                         (list (eask-install-file--guess-name file) file))
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
    (eask-info "(No file packages have been intalled)")
    (eask-help "core/install-file")))

;;; core/install-file.el ends here

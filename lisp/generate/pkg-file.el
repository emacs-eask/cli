;;; generate/pkg-file.el --- Generate -pkg file  -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; Command use generate -pkg file,
;;
;;   $ eask generate pkg-file
;;

;;; Code:

(let ((dir (file-name-directory (nth 1 (member "-scriptload" command-line-args)))))
  (load (expand-file-name "_prepare.el"
                          (locate-dominating-file dir "_prepare.el"))
        nil t))

(defvar eask-generate-pkg-file--filename)

(defun eask-generate-pkg-file--from-pkg-desc ()
  "Generate pkg-file from a package-descriptor."
  (let* ((name (package-desc-name eask-package-desc))
         (pkg-file (expand-file-name (format "%s-pkg.el" name))))
    (setq eask-generate-pkg-file--filename pkg-file)
    (package-generate-description-file eask-package-desc pkg-file)))

(defun eask-generate-pkg-file--from-eask-file ()
  "Generate pkg-file from Eask file."
  (let* ((name (eask-guess-package-name))
         (pkg-file (expand-file-name (concat name "-pkg.el")))
         (version (eask-package-version))
         (description (eask-package-description))
         (reqs (mapcar (lambda (elm)
                         (list (eask-intern (car elm))
                               (if (= (length (cdr elm)) 1)
                                   (nth 0 (cdr elm))
                                 "0")))
                       (append eask-depends-on-emacs eask-depends-on))))
    (setq eask-generate-pkg-file--filename pkg-file)
    (write-region
     (pp-to-string `(define-package ,name ,version ,description ',reqs))
     nil pkg-file)))

(eask-start
  (if eask-package-desc (eask-generate-pkg-file--from-pkg-desc)
    (eask-generate-pkg-file--from-eask-file))
  (eask-info "%s:" (file-name-nondirectory (directory-file-name eask-generate-pkg-file--filename)))
  (eask-msg "")
  (eask-msg "%s" (with-temp-buffer
                   (emacs-lisp-mode)
                   (insert-file-contents eask-generate-pkg-file--filename)
                   (pp-buffer)
                   (eask--silent (indent-region (point-min) (point-max)))
                   (buffer-string)))
  (eask-info "(Generated -pkg.el file in %s)" eask-generate-pkg-file--filename))

;;; generate/pkg-file.el ends here

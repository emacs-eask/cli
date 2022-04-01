;;; package.el --- External module `package'  -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; The basic extensions.
;;

;;; Code:

(unless (fboundp 'package-get-descriptor)

  (defun package--get-activatable-pkg (pkg-name)
    ;; Is "activatable" a word?
    (let ((pkg-descs (cdr (assq pkg-name package-alist))))
      ;; Check if PACKAGE is available in `package-alist'.
      (while
          (when pkg-descs
            (let ((available-version (package-desc-version (car pkg-descs))))
              (or (package-disabled-p pkg-name available-version)
                  ;; Prefer a builtin package.
                  (package-built-in-p pkg-name available-version))))
        (setq pkg-descs (cdr pkg-descs)))
      (car pkg-descs)))

  (defun package-get-descriptor (pkg-name)
    "Return the `package-desc' of PKG-NAME."
    (unless package--initialized (package-initialize 'no-activate))
    (or (package--get-activatable-pkg pkg-name)
        (cadr (assq pkg-name package-alist))
        (cadr (assq pkg-name package-archive-contents))))
  )

;;; package-build.el ends here

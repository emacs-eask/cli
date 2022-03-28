;;; test-redefine.el --- Test redefine  -*- lexical-binding: t; -*-

;;; Commentary:
;;
;;
;;

;;; Code:

(setq byte-compile-error-on-warn t)

(let* ((concated-file "./dist/eask.built.el")
       (concated-file (expand-file-name concated-file))
       (test-conditions "
;; Local Variables:
;; coding: utf-8
;; byte-compile-warnings: (redefine)
;; End:
"))
  (with-current-buffer (find-file concated-file)
    (unless (string-suffix-p test-conditions (buffer-string))
      (goto-char (point-max))
      (insert test-conditions))
    (byte-compile-file buffer-file-name)))

;;; test-redefine.el ends here

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
;; byte-compile-warnings: (redefine)
;; End:
"))
  (with-current-buffer (find-file concated-file)
    (insert-file-contents concated-file)
    (unless (string-suffix-p test-conditions (buffer-string))
      (insert test-conditions))
    (byte-compile-file buffer-file-name)))

;;; test-redefine.el ends here

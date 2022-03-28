;;; test-redefine.el --- Test redefine  -*- lexical-binding: t; -*-

;;; Commentary:
;;
;;
;;

;;; Code:

(let* ((concated-file "./dist/eask.built.el")
       (concated-file (expand-file-name concated-file)))
  (write-region
   "
;; Local Variables:
;; byte-compile-warnings: (redefine)
;; End:
" nil concated-file t))

;;; test-redefine.el ends here

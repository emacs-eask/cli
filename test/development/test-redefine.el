;;; test-redefine.el --- Test redefine  -*- lexical-binding: t; -*-

;;; Commentary:
;;
;;
;;

;;; Code:

(setq byte-compile-error-on-warn t)

(let ((concated-file "./dist/eask.built.el"))
  (write-region
   "
;; Local Variables:
;; byte-compile-warnings: (redefine)
;; End:
" nil concated-file t)

  (message "")
  (message "Compile file %s..." concated-file)
  (byte-compile-file concated-file))

;;; test-redefine.el ends here

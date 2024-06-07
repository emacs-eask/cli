;;; core/recompile.el --- Byte-recompile `.el' files  -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; Byte-recompile `.el' files,
;;
;;   $ eask recompile [names..]
;;
;;
;;  Positionals:
;;
;;    [names..]     specify files to byte-recompile
;;

;;; Code:

(let ((dir (file-name-directory (nth 1 (member "-scriptload" command-line-args)))))
  (load (expand-file-name "_prepare.el"
                          (locate-dominating-file dir "_prepare.el"))
        nil t))

;;
;;; Core

(eask-start
  (eask-call "clean/elc")
  (eask-write "")
  (eask-call "core/compile"))

;;; core/recompile.el ends here

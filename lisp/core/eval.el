;;; core/eval.el --- Evaluate lisp form with a proper PATH  -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; Evaluate lisp form with a proper PATH,
;;
;;   $ eask eval [form]
;;

;;; Code:

(let ((dir (file-name-directory (nth 1 (member "-scriptload" command-line-args)))))
  (load (expand-file-name "_prepare.el"
                          (locate-dominating-file dir "_prepare.el"))
        nil t))

(eask-load "core/exec")

(eask-start
  (eask-defvc< 27 (eask-pkg-init))  ; XXX: remove this after we drop 26.x
  (eask-setup-paths)

  (if-let ((name (eask-argv 0)))
      (eask-with-progress
        (ansi-green "Exporting environment variables... ")
        (eask--export-env)
        (ansi-green "done ✓"))
    (eask-info "(No expression found)")
    (eask-help "core/eval")))

;;; core/eval.el ends here

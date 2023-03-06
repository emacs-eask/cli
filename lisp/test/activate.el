;;; test/activate.el --- activate package  -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; Command use to activate package
;;
;;   $ eask test activate
;;

;;; Code:

(let ((dir (file-name-directory (nth 1 (member "-scriptload" command-line-args)))))
  (load (expand-file-name "_prepare.el"
                          (locate-dominating-file dir "_prepare.el"))
        nil t))

(eask-start
  (eask-pkg-init)
  (let ((name (eask-guess-package-name)))
    (eask-with-progress
      (format "Activating the package `%s'... " (ansi-green name))
      (require (intern name))
      "succeeded ✓"))
  ;; XXX: Call `core/load' command to load the files for activation tests!
  ;; The command will parse the argument itself, so we don't have to worry!
  (eask-call "core/load"))

;;; test/activate.el ends here

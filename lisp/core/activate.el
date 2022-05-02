;;; activate.el --- activate package  -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; Command use to activate package
;;
;;   $ eask activate
;;

;;; Code:

(load (expand-file-name
       "../_prepare.el"
       (file-name-directory (nth 1 (member "-scriptload" command-line-args))))
      nil t)

(eask-start
  (eask-pkg-init)
  (let ((name (eask-guess-package-name)))
    (eask-with-progress
      (format "Activating the package `%s'... " (ansi-green name))
      (require (intern name))
      "succeeded ✓"))
  (eask-call "core/load"))

;;; activate.el ends here

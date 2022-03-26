;;; build.el --- Build package into installable tar file  -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; Command use to build package into installable tar file
;;
;;   $ eask build
;;

;;; Code:

(load-file (expand-file-name
            "_prepare.el"
            (file-name-directory (nth 1 (member "-scriptload" command-line-args)))))

(eask-start
  (eask-pkg-init)
  (eask-package-install 'package-build)
  (if (eask-package-multi-p)
      ()
    (message "\n No need to build single file packages")))

;;; build.el ends here

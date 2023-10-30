;;; test/melpazoid.el --- Run melpazoid tests  -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; Command to run melpazoid tests,
;;
;;   $ eask melpazoid
;;

;;; Code:

(let ((dir (file-name-directory (nth 1 (member "-scriptload" command-line-args)))))
  (load (expand-file-name "_prepare.el"
                          (locate-dominating-file dir "_prepare.el"))
        nil t))

;;
;;; Flags

(advice-add #'eask-allow-error-p :override #'always)

;;
;;; Core

(defcustom eask-melpazoid-el-url
  "https://raw.githubusercontent.com/riscy/melpazoid/master/melpazoid/melpazoid.el"
  "Url path to melpazoid's elisp file."
  :type 'string
  :group 'eask)

(eask-start
  ;; Preparation
  (eask-with-archives "melpa"
    (eask-package-install 'package-lint)
    (eask-package-install 'pkg-info))
  ;; Start test
  (eask-import eask-melpazoid-el-url))

;;; test/melpazoid.el ends here

;;; test/melpazoid.el --- Run melpazoid tests  -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; Command to run melpazoid tests,
;;
;;   $ eask test melpazoid
;;

;;; Code:

(let ((dir (file-name-directory (nth 1 (member "-scriptload" command-line-args)))))
  (load (expand-file-name "_prepare.el"
                          (locate-dominating-file dir "_prepare.el"))
        nil t))

;;
;;; Flags

(advice-add #'eask-allow-error-p :override #'eask-always)

;;
;;; Core

(defcustom eask-test-melpazoid-el-url
  "https://raw.githubusercontent.com/riscy/melpazoid/master/melpazoid/melpazoid.el"
  "Url path to melpazoid's elisp file."
  :type 'string
  :group 'eask)

(eask-start
  ;; Preparation
  (eask-archive-install-packages '("gnu" "melpa")
                                 'package-lint
                                 'pkg-info)

  ;; Start Testing
  (let* ((dirs (or (eask-args) `(,default-directory))))
    (cond
     ;; Files found, do the action!
     (dirs
      (dolist (dir dirs)
        (let ((default-directory (expand-file-name dir)))
          (eask-info "[+] %s" default-directory)
          (eask-import eask-test-melpazoid-el-url))))
     ;; Default, print help!
     (t
      (eask-info "(No tests found.)")
      (eask-help "test/melpazoid")))))

;;; test/melpazoid.el ends here

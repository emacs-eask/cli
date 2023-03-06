;;; clean/pkg-file.el --- Remove generated pkg-file  -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; Remove generated pkg-file,
;;
;;   $ eask clean pkg-file
;;

;;; Code:

(let ((dir (file-name-directory (nth 1 (member "-scriptload" command-line-args)))))
  (load (expand-file-name "_prepare.el"
                          (locate-dominating-file dir "_prepare.el"))
        nil t))

(eask-start
  (let* ((name (eask-guess-package-name))
         (pkg-file (expand-file-name (concat name "-pkg.el")))
         (deleted (eask-delete-file pkg-file)))
    (eask-msg "")
    (if deleted
        (eask-info "(Total of 1 file deleted)")
      (eask-info "(No pkg-file found in workspace)")
      (setq eask-no-cleaning-operation-p t))))

;;; clean/pkg-file.el ends here

;;; clean/autoloads.el --- Remove generated autoloads file  -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; Remove generated autoloads file,
;;
;;   $ eask clean autoloads
;;

;;; Code:

(let ((dir (file-name-directory (nth 1 (member "-scriptload" command-line-args)))))
  (load (expand-file-name "_prepare.el"
                          (locate-dominating-file dir "_prepare.el"))
        nil t))

(eask-start
  (let* ((name (eask-guess-package-name))
         (autoloads-file (expand-file-name (concat name "-autoloads.el")))
         (deleted (eask-delete-file autoloads-file)))
    (eask-msg "")
    (if deleted
        (eask-info "(Total of 1 file deleted)")
      (eask-info "(No autoloads file found in workspace)")
      (setq eask-no-cleaning-operation-p t))))

;;; clean/autoloads.el ends here

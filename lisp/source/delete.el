;;; source/delete.el --- Remove an archive source  -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; Commmand use to remove an archive source
;;
;;   $ eask source delete <name>
;;
;;
;;  Positionals:
;;
;;    <name>     name of the archive
;;

;;; Code:

(let ((dir (file-name-directory (nth 1 (member "-scriptload" command-line-args)))))
  (load (expand-file-name "_prepare.el"
                          (locate-dominating-file dir "_prepare.el"))
        nil t))

(defun eask-source-delete (name)
  "Delete an archive source by NAME."
  (with-current-buffer (find-file eask-file)
    (goto-char (point-min))
    (when (re-search-forward (concat "([ \t\n\r]*source[ \t\n\r]*['\"]+" name) nil t)
      (delete-region (line-beginning-position) (1+ (line-end-position)))
      (eask-info "(Delete source `%s')" name))
    (save-buffer)))

(eask-start
  (let* ((args (eask-args))
         (name (nth 0 args)))
    (if-let* ((exists (assoc name package-archives)))
        (eask-source-delete name)
      (eask-info "(Invalid source name, `%s')" name)
      (eask-help "source/delete"))))

;;; source/delete.el ends here

;;; init/cask.el --- Initialize Eask from Cask  -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; Commmand use to convert Cask-file to Eask-file
;;
;;   $ eask init --from cask
;;

;;; Code:

(load (expand-file-name
       "../_prepare.el"
       (file-name-directory (nth 1 (member "-scriptload" command-line-args))))
      nil t)

(defun eask--convert-cask (filename)
  "Convert Cask FILENAME to Eask."
  (let* ((filename (expand-file-name filename))
         (file (eask-root-del filename))
         (new-file (eask-s-replace "Cask" "Eask" file))
         (new-filename (expand-file-name new-file))
         (converted))
    (eask-with-progress
      (format "Converting file %s to %s... " file new-file)
      (eask-with-verbosity 'debug
        (cond ((not (string-prefix-p "Cask" file))
               (eask-debug "✗ Invalid Cask filename, the file should start with `Cask`"))
              ((file-exists-p new-filename)
               (eask-debug "✗ The file `%s` already presented" new-file))
              (t
               (with-current-buffer (find-file new-filename)
                 (insert-file-contents file)
                 (goto-char (point-min))
                 (while (re-search-forward "(source " nil t)
                   (insert "'"))  ; make it symbol
                 (save-buffer))
               (setq converted t))))
      (if converted "done ✓" "skipped ✗"))
    converted))

(eask-start
  (let* ((patterns (eask-args))
         (files (if patterns
                    (eask-expand-file-specs patterns)
                  (directory-files default-directory t "/Cask\\'")))
         (converted 0))
    (cond
     ;; Files found, do the action!
     (files
      (dolist (file files)
        (when (eask--convert-cask file)
          (cl-incf converted)))
      (eask-msg "")
      (eask-info "(Total of %s Cask-file%s converted)" converted
                 (eask--sinr converted "" "s")))
     ;; Pattern defined, but no file found!
     (patterns
      (eask-msg "")
      (eask-info "No files found with wildcard pattern: %s"
                 (mapconcat #'identity patterns " ")))
     ;; Default, print help!
     (t
      (eask-msg "")
      (eask-info "(No Cask-files have been converted to Eask)")
      (eask-help "init/cask")))))

;;; init/cask.el ends here

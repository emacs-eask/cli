;;; core/status.el --- Display the state of the workspace  -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; Display the state of the workspace
;;
;;   $ eask status
;;

;;; Code:

(let ((dir (file-name-directory (nth 1 (member "-scriptload" command-line-args)))))
  (load (expand-file-name "_prepare.el"
                          (locate-dominating-file dir "_prepare.el"))
        nil t))

(eask-start
  (eask-msg "✓ Emacs version %s" emacs-version)
  (eask-with-verbosity 'debug
    (eask-msg "  💡 Invoke from %s" invocation-directory)
    (eask-msg "  💡 Build No. %s" emacs-build-number)
    (eask-msg "  💡 System configuration %s" system-configuration)
    (when-let ((emacs-build-time)
               (time (format-time-string "%Y-%m-%d" emacs-build-time)))
      (eask-msg "  💡 Build time %s" time)))
  (eask-msg "✓ System type %s" system-type)
  (eask-msg "✓ Emacs directory %s" user-emacs-directory)
  (eask-with-verbosity 'debug
    (eask-msg "  💡 ELPA directory %s" package-user-dir))
  (if eask-file
      (eask-msg "✓ Eask file in %s" eask-file)
    (eask-msg "✗ Eask file... missing!"))

  (eask-msg "")
  (eask-info "(Total of %s states listed)" (if (eask--reach-verbosity-p 'debug)
                                               9
                                             4)))

;;; core/status.el ends here

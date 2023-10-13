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

;;
;;; Externals

(declare-function ansi-bright-black "ext:ansi.el")
(declare-function ansi-underscore "ext:ansi.el")

;;
;;; Core

(defun eask--environment-name ()
  "Get the working environment name."
  (cond ((eask-global-p) "global (~/)")
        ((eask-config-p) (format "configuration (%s)" user-emacs-directory))
        (t               "development (./)")))

(defun eask--print-title (title)
  "Print section TITLE."
  (eask-msg "")
  (eask-msg (ansi-underscore title))
  (eask-msg ""))

(defun eask--print-info (pair)
  "Print environment info PAIR."
  (let ((title   (eask-2str (car pair)))
        (content (eask-2str (cdr pair))))
    (eask-msg "   %-22s %s" title (ansi-bright-black content))))

(eask-start
  (eask-msg "In the %s environment" (eask--environment-name))
  (eask-msg "Your emacs home is point to %s" user-emacs-directory)

  (eask--print-title "System:")
  (eask--print-info `("Emacs version" . ,emacs-version))
  (eask--print-info `("Invocation" . ,invocation-directory))
  (eask--print-info `("Build No." . ,emacs-build-number))
  (eask--print-info `("System configuration" . ,system-configuration))
  (when-let ((emacs-build-time)
             (time (format-time-string "%Y-%m-%d" emacs-build-time)))
    (eask--print-info `("Build time" . ,time)))
  (eask--print-info `("System type" . ,system-type))

  (eask--print-title "Environment:")
  (eask--print-info `("Emacs directory" . ,user-emacs-directory))
  (eask--print-info `("ELPA directory" . ,package-user-dir))
  (eask--print-info `("early-init.el" . ,early-init-file))
  (eask--print-info `("init.el" . ,user-init-file))
  (eask--print-info `("custom.el" . ,custom-file))

  (eask--print-title "Eask-file:")
  (eask--print-info `("Eask file" . ,(or eask-file "missing")))
  (eask--print-info `("Eask-file Count" . ,(length (eask--find-files default-directory))))

  (eask-msg "")
  ;; XXX: Please increment the number everytime a new information is added!
  (eask-info "(Total of %s states listed)" (+ 6 5 2)))

;;; core/status.el ends here

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

(defvar eask--status-info-count 0
  "Count of the stratus info.")

(defun eask--environment-name ()
  "Get the working environment name."
  (cond ((eask-global-p) "global (~/)")
        ((eask-config-p) (format "configuration (%s)" user-emacs-directory))
        (t               "development (./)")))

(defun eask--print-title (title)
  "Print section TITLE."
  (eask-println "")
  (eask-println (ansi-underscore title))
  (eask-println ""))

(defun eask--print-info (fmt pair)
  "Print environment info with FMT and PAIR."
  (let ((title   (eask-2str (nth 0 pair)))
        (content (eask-2str (nth 1 pair)))
        (note    (eask-2str (or (nth 2 pair) ""))))
    (eask-println fmt
                  title
                  (ansi-bright-black content)
                  note)))

(defun eask--list-max-length (lst index)
  "Return the LST max length by its INDEX."
  (let ((max-len 0)
        (max-current))
    (dolist (data lst)
      (setq max-current (eask-2str (nth index data))
            max-current (pcase index
                          (1 (ansi-bright-black max-current))
                          (_ max-current))
            max-len (max (length max-current) max-len)))
    max-len))

(defun eask--print-infos (lst)
  "Print environment info LST."
  (let* ((len-0 (eask-2str (eask--list-max-length lst 0)))
         (len-1 (eask-2str (+ (eask--list-max-length lst 1) 2)))
         (fmt (concat "   %-21s   %-" len-1 "s   %s")))
    (dolist (pair lst)
      (when pair
        (eask--print-info fmt pair)
        (cl-incf eask--status-info-count)))))

(defun eask--status-file-dir (path)
  "Return file directory status from PATH."
  (unless (file-exists-p path)
    (ansi-red "(missing)")))

(eask-start
  (eask-println "In the %s environment" (eask--environment-name))
  (eask-println "Your emacs home is point to %s" (expand-file-name user-emacs-directory))

  (eask--print-title "System:")
  (eask--print-infos
   `(("Emacs version" ,emacs-version)
     ("Invocation" ,invocation-directory)
     ("Build No." ,emacs-build-number)
     ("System configuration" ,system-configuration)
     ,(when-let ((emacs-build-time)
                 (time (format-time-string "%Y-%m-%d" emacs-build-time)))
        `("Build time" ,time))
     ("System type" ,system-type)))

  (eask--print-title "Environment:")
  (let ((dot-emacs-file (locate-user-emacs-file "../.emacs")))
    (eask--print-infos
     `(("Emacs directory" ,(expand-file-name user-emacs-directory)
        ,(eask--status-file-dir user-emacs-directory))
       ("ELPA directory" ,(expand-file-name package-user-dir)
        ,(eask--status-file-dir package-user-dir))
       ("early-init.el" ,(expand-file-name early-init-file)
        ,(eask--status-file-dir early-init-file))
       (".emacs" ,(expand-file-name dot-emacs-file)
        ,(eask--status-file-dir dot-emacs-file))
       ("init.el" ,(expand-file-name user-init-file)
        ,(eask--status-file-dir user-init-file))
       ("custom.el" ,(if custom-file (expand-file-name custom-file)
                       "nil")
        ,(when custom-file (eask--status-file-dir custom-file))))))

  (eask--print-title "Eask-file:")
  (eask--print-infos
   `(("Eask file" ,(or eask-file "missing"))
     ("Eask-file Count" ,(length (eask--find-files default-directory)))))

  (eask-info "(Total of %s states listed)" eask--status-info-count))

;;; core/status.el ends here

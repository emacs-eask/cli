;;; _prepare.el --- Prepare for command tasks  -*- lexical-binding: t; -*-
;;; Commentary: Utility module to make Eask work
;;; Code:

(require 'package)
(require 'rect)
(require 'subr-x)

;;
;;; Flag

(defun eask--str2num (str) (ignore-errors (string-to-number str)))

(defun eask--flag (flag)
  "Return non-nil if FLAG exists.."
  (member (concat "--eask" flag) argv))

(defun eask--flag-value (flag)
  "Return value for FLAG."
  (nth 1 (eask--flag flag)))

;;; Boolean
(defun eask-global-p () (eask--flag "-g"))  ; -g is enabled
(defun eask-force-p ()  (eask--flag "-f"))  ; -f is enabled

;;; String
;; TODO: n/a

;;; Number
(defun eask-depth ()
  (eask--str2num (eask--flag-value "-depth")))

;;
;;; Core

(defconst eask--command-list
  '("--eask-g" "--eask-f" "--eask-depth")
  "List of commands to accept, so we can avoid unknown option error.")

(defmacro eask--setup-env (&rest body)
  "Execute BODY with workspace setup."
  (declare (indent 0) (debug t))
  `(let* ((alist)
          (_ (dolist (cmd eask--command-list)
               (push (cons cmd '(lambda (&rest _))) alist))))
     (setq command-switch-alist (append command-switch-alist alist))
     ,@body))

(defmacro eask-start (&rest body)
  "Execute BODY with workspace setup."
  (declare (indent 0) (debug t))
  `(eask--setup-env
     ;; set it locally, else we ignore to respect default settings
     (if (eask-global-p) (progn ,@body)
       (let* ((user-emacs-directory (expand-file-name ".eask/"))
              (package-user-dir (expand-file-name "elpa" user-emacs-directory))
              (user-init-file (locate-user-emacs-file "init.el"))
              (custom-file (locate-user-emacs-file "custom.el"))
              (eask-file (expand-file-name "../Eask" user-emacs-directory)))
         (ignore-errors (make-directory package-user-dir t))
         (ignore-errors (load-file eask-file))
         ,@body))))

;;; _prepare.el ends here

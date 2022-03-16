;;; _prepare.el --- Prepare for command tasks  -*- lexical-binding: t; -*-
;;; Commentary: Utility module to make Eask work
;;; Code:

(require 'package)
(require 'project)

(require 'cl-lib)
(require 'rect)
(require 'subr-x)

(setq package-enable-at-startup nil  ; To avoid initializing twice
      package-check-signature nil)

(setq package-archives nil
      package-archive-priorities nil)

(defcustom eask-path-ignores
  (append project-vc-ignores '(".eask"))
  "List of default path to ignore for search result."
  :type 'list
  :group 'eask)

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

(defvar eask--first-init-p nil
  "Is non-nil if .eask does not exists; meaning users haven't called eask in the
current workspace.")

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

(defun eask-fbound (symbol) (and (fboundp symbol) symbol))

(defun eask--keywords-update (package package-file files depends-on development source source-priority)
  "Update set of Eask file functions."
  (defalias 'package         (symbol-function package))
  (defalias 'package-file    (symbol-function package-file))
  (defalias 'files           (symbol-function files))
  (defalias 'depends-on      (symbol-function depends-on))
  (defalias 'development     (symbol-function development))
  (defalias 'source          (symbol-function source))
  (defalias 'source-priority (symbol-function source-priority)))

(defun eask-file-load ()
  "Load Eask file in workspace."
  (let ((eask-file (expand-file-name "../../Eask" user-emacs-directory))
        (f-package         (eask-fbound 'package))
        (f-package-file    (eask-fbound 'package-file))
        (f-files           (eask-fbound 'files))
        (f-depends-on      (eask-fbound 'depends-on))
        (f-development     (eask-fbound 'development))
        (f-source          (eask-fbound 'source))
        (f-source-priority (eask-fbound 'source-priority)))
    (eask--keywords-update #'eask-package #'eask-package-file #'eask-files
                           #'eask-depends-on #'eask-development
                           #'eask-source #'eask-source-priority)
    (load-file eask-file)
    (eask--keywords-update f-package f-package-file f-files
                           f-depends-on f-development
                           f-source f-source-priority)))

(defmacro eask-start (&rest body)
  "Execute BODY with workspace setup."
  (declare (indent 0) (debug t))
  `(eask--setup-env
     ;; set it locally, else we ignore to respect default settings
     (if (eask-global-p) (progn ,@body)
       (let* ((user-emacs-directory (expand-file-name (concat ".eask/" emacs-version "/")))
              (package-user-dir (expand-file-name "elpa" user-emacs-directory))
              (eask--first-init-p (not (file-directory-p user-emacs-directory)))
              (user-init-file (locate-user-emacs-file "init.el"))
              (custom-file (locate-user-emacs-file "custom.el")))
         (ignore-errors (make-directory package-user-dir t))
         (eask-file-load)
         ,@body))))

;;
;;; Eask file

(defconst eask-source-mapping
  `((gnu          . ,(concat (if (< emacs-major-version 27) "http" "https")
                             "://elpa.gnu.org/packages/"))
    (celpa        . "https://celpa.conao3.com/packages/")
    (jcs-elpa     . "https://jcs-emacs.github.io/jcs-elpa/packages/")
    (melpa        . "https://melpa.org/packages/")
    (melpa-stable . "https://stable.melpa.org/packages/")
    (marmalade    . "https://marmalade-repo.org/packages/")
    (nongnu       . "https://elpa.nongnu.org/nongnu/")
    (org          . "https://orgmode.org/elpa/"))
  "Mapping of source name and url.")

(defvar eask-package-file nil)
(defvar eask-files nil)

(defun eask-package (name version description)
  ""
  ;; TODO: ..
  )

(defun eask-package-file (file)
  "Set package file."
  (setq eask-package-file (expand-file-name file)))

(defun eask-files (&rest patterns)
  "Set files patterns."
  (setq eask-files patterns))

(defun eask-depends-on (pkg &optional minimum-version)
  "Specify a dependency of this package.")

(defun eask-development (file)
  ""
  ;; TODO: ..
  )

(defun eask-source (name &optional location)
  "Add archive NAME with LOCATION."
  (setq location (or location (cdr (assq (intern name) eask-source-mapping))))
  (unless location (error "Unknown package archive: %s" name))
  (push (cons name location) package-archives))

(defun eask-source-priority (archive-id &optional priority)
  "Add PRIORITY for to ARCHIVE-ID."
  (push (cons archive-id priority) package-archive-priorities))

;;
;;; Error Handling

(defun eask--exit (&rest _) "Send exit code." (kill-emacs 1))

(advice-add 'error :after #'eask--exit)

;;; _prepare.el ends here

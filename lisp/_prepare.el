;;; _prepare.el --- Prepare for command tasks  -*- lexical-binding: t; -*-
;;; Commentary: Utility module to make Eask work
;;; Code:

(require 'package)
(require 'project)

(require 'cl-lib)
(require 'rect)
(require 'subr-x)

(setq package-enable-at-startup nil    ; To avoid initializing twice
      package-check-signature nil)

(setq package-archives nil             ; Leave it to custom use
      package-archive-priorities nil)

(defcustom eask-path-ignores
  (append project-vc-ignores '(".eask"))
  "List of default path to ignore for search result."
  :type 'list
  :group 'eask)

;;
;;; Package

(defun eask-pkg-init ()
  "Package initialization."
  (package-initialize)
  (package-refresh-contents))

(defun eask-package-install (pkg)
  "Install the package PKG."
  (package-initialize)
  (let ((pkg (if (stringp pkg) (intern pkg) pkg)))
    (unless (package-installed-p pkg)
      (package-refresh-contents)
      (package-install pkg))))

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
(defun eask-global-p () (eask--flag "-g"))     ; -g   is enabled
(defun eask-force-p ()  (eask--flag "-f"))     ; -f   is enabled
(defun eask-dev-p ()    (eask--flag "--dev"))  ; -dev is enabled

;;; String (with arguments)
;; XXX Add string argument here if any!

;;; Number (with arguments)
(defun eask-depth () (eask--str2num (eask--flag-value "--depth")))  ; -depth is enabled

;;
;;; Execution

(defconst eask--script (nth 1 (member "-scriptload" command-line-args))
  "Script currently executing.")

(defun eask-command ()
  "What's the current command?"
  (file-name-sans-extension (file-name-nondirectory eask--script)))

(defun eask-call (script)
  "Call another eask script."
  (let ((script-el (concat script ".el"))
        (lisp-dir (file-name-directory eask--script)))
    (load-file (expand-file-name script-el lisp-dir))))

;;
;;; Core

(defvar eask--first-init-p nil
  "Is non-nil if .eask does not exists; meaning users haven't called eask in the
current workspace.")

(defconst eask--command-list
  '("--eask-g" "--eask-f" "--eask--depth" "--eask--dev")
  "List of commands to accept, so we can avoid unknown option error.")

(defun eask-self-command-p (arg)
  "Return non-nil if ARG is known internal command."
  (member arg eask--command-list))

(defun eask-argv ()
  "Get all arguments except options."
  (cl-remove-if #'eask-self-command-p argv))

(defmacro eask--setup-env (&rest body)
  "Execute BODY with workspace setup."
  (declare (indent 0) (debug t))
  `(let* ((alist)
          (_ (dolist (cmd eask--command-list)
               (push (cons cmd '(lambda (&rest _))) alist))))
     (setq command-switch-alist (append command-switch-alist alist))
     ,@body))

(defun eask-fbound (symbol) (and (fboundp symbol) symbol))

(defconst eask-file-keywords
  '("package" "package-file" "files"
    "depends-on" "development"
    "source" "source-priority"
    "load-path" "load-paths")
  "List of Eask file keywords.")

(defun eask--loop-file-keywords (func)
  "Loop through Eask file keywords for environment replacement.  Internal used
for function `eask--alias-env'."
  (dolist (keyword eask-file-keywords)
    (let ((keyword-sym (intern keyword))
          (api (intern (concat "eask-" keyword)))      ; existing function
          (old (intern (concat "eask--f-" keyword))))  ; variable that holds function pointer
      (funcall func keyword-sym api old))))

(defmacro eask--alias-env (&rest body)
  "Replace all Eask file functions temporary; this is only used when loading
Eask file in the workspace."
  (declare (indent 0) (debug t))
  `(progn
     (eask--loop-file-keywords
      (lambda (keyword api old)
        (eval `(defvar ,old nil))
        (eval `(setq ,old (eask-fbound (quote ,keyword))))
        (eval `(defalias (quote ,keyword) (quote ,api)))))
     (progn ,@body)
     (eask--loop-file-keywords
      (lambda (keyword api old)
        ;; TODO: Revert the original function's definition; just in case,
        ;; anything else don't go wrong
        (eval `(defalias (quote ,keyword) (symbol-function ,old)))))))

(defvar eask-file nil "The Eask file path.")

(defcustom eask-before-command-hook nil
  "Hooks run before any command is executed."
  :type 'hook
  :group 'eask)

(defcustom eask-after-command-hook nil
  "Hooks run after any command is executed."
  :type 'hook
  :group 'eask)

(defmacro eask-start (&rest body)
  "Execute BODY with workspace setup."
  (declare (indent 0) (debug t))
  `(eask--setup-env
     (run-hooks 'eask-before-command-hook)
     (run-hooks (intern (concat "eask-before-command-" (eask-command) "-hook")))
     ;; set it locally, else we ignore to respect default settings
     (if (eask-global-p) (progn ,@body)
       (let* ((user-emacs-directory (expand-file-name (concat ".eask/" emacs-version "/")))
              (package-user-dir (expand-file-name "elpa" user-emacs-directory))
              (eask--first-init-p (not (file-directory-p user-emacs-directory)))
              (user-init-file (locate-user-emacs-file "init.el"))
              (custom-file (locate-user-emacs-file "custom.el")))
         (setq eask-file (expand-file-name "../../Eask" user-emacs-directory))
         (ignore-errors (make-directory package-user-dir t))
         (eask--alias-env (load-file eask-file))
         ,@body))
     (run-hooks (intern (concat "eask-after-command-" (eask-command) "-hook")))
     (run-hooks 'eask-after-command-hook)))

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

(defvar eask-package nil)
(defvar eask-package-file nil)
(defvar eask-files nil)
(defvar eask-depends-on nil)
(defvar eask-depends-on-dev nil)

(defvar eask--development-scope nil)

(defun eask-package (name version description)
  "Set the package information."
  (setq eask-package `(:name ,name :version ,version :description ,description)))

(defun eask-package-file (file)
  "Set package file."
  (setq eask-package-file (expand-file-name file)))

(defun eask-files (&rest patterns)
  "Set files patterns."
  (setq eask-files patterns))

(defun eask-depends-on (pkg &optional minimum-version)
  "Specify a dependency of this package."
  (push pkg eask-depends-on)
  (delete-dups eask-depends-on)
  pkg)

(defun eask-development (&rest dep)
  "Development scope."
  (dolist (pkg dep)
    (push pkg eask-depends-on-dev)
    (delete-dups eask-depends-on-dev)
    (setq eask-depends-on (remove pkg eask-depends-on))))  ; remove it from production

(defun eask-load-path (dir)
  "Add DIR to load-path."
  (add-to-list 'load-path (expand-file-name dir default-directory)))

(defun eask-load-paths (&rest dirs)
  "Add all DIRS to load-path."
  (dolist (dir dirs) (eask-load-path dir)))

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

;;
;;; File

(defun eask--filter-exclude-dirs (item)
  "Filter out ITEM from default ignore paths."
  (not (cl-some (lambda (elm) (string-match-p elm item)) eask-path-ignores)))

(defun eask--f-entries (path pattern)
  "Return entries from PATH with PATTERN."
  (when (file-directory-p path)
    (cl-remove-if-not
     (lambda (file) (string-match-p pattern file))
     (directory-files-recursively path "^\\([^.]\\|\\.[^.]\\|\\.\\..\\)" nil
                                  #'eask--filter-exclude-dirs))))

(defun eask-package-files ()
  "Return package files in workspace."
  (let (files)
    (dolist (pattern eask-files)
      (setq files (append files (eask--f-entries default-directory pattern))))
    ;; Package file is part of package-files
    (when eask-package-file (push eask-package-file files))
    (delete-dups files)))

(defun eask-package-el-files ()
  "Return package files in workspace."
  (cl-remove-if-not (lambda (filename) (string= (file-name-extension filename) "el")) (eask-package-files)))

;;; _prepare.el ends here

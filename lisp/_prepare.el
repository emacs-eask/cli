;;; _prepare.el --- Prepare for command tasks  -*- lexical-binding: t; -*-
;;; Commentary: Prepare to setup Eask environment for sandboxing
;;; Code:

(require 'package)
(require 'project)
(require 'url-vars)

(require 'cl-lib)
(require 'pp)
(require 'rect)
(require 'subr-x)

(unless (bound-and-true-p eask--initialized-p)
  (setq package-enable-at-startup  nil            ; To avoid initializing twice
        package-check-signature    nil
        package-archives           nil            ; Leave it to custom use
        package-archive-priorities nil))

(defun eask--load-file--adv (fnc &rest args)
  "Prevent `_prepare.el' loading twice."
  (unless (string= (nth 0 args) (eask-script "_prepare")) (apply fnc args)))
(advice-add 'load-file :around #'eask--load-file--adv)

;;
;;; Package

(defun eask--update-exec-path ()
  "Add all bin directory to `exec-path'."
  (dolist (filename (directory-files-recursively package-user-dir "^\\([^.]\\|\\.[^.]\\|\\.\\..\\)"))
    (when (string-suffix-p "bin/" (file-name-directory filename))
      (add-to-list 'exec-path (file-name-directory filename))))
  (delete-dups exec-path))

(defun eask--update-load-path ()
  "Add all load-path for all .el files."
  (dolist (filename (eask-package-el-files))
    (add-to-list 'load-path (file-name-directory filename)))
  (delete-dups load-path))

(defun eask-pkg-init ()
  "Package initialization."
  (package-initialize)
  (package-refresh-contents)
  (eask--update-exec-path)
  (eask--update-load-path))

(defun eask-package-install (pkg)
  "Install the package PKG."
  (package-initialize)
  (let ((pkg (if (stringp pkg) (intern pkg) pkg)))
    (unless (package-installed-p pkg)
      (package-refresh-contents)
      (package-install pkg))
    (require pkg nil t)))

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
(defun eask-global-p () (eask--flag "-g"))       ; -g, --global
(defun eask-force-p ()  (eask--flag "-f"))       ; -f, --force
(defun eask-dev-p ()    (eask--flag "--dev"))    ; --dev, --development
(defun eask-debug-p ()  (eask--flag "--debug"))  ; --debug

;;; String (with arguments)
(defun eask-proxy ()       (eask--flag-value "--proxy"))
(defun eask-http-proxy ()  (eask--flag-value "--http-proxy"))
(defun eask-https-proxy () (eask--flag-value "--https-proxy"))
(defun eask-no-proxy ()    (eask--flag-value "--no-proxy"))

;;; Number (with arguments)
(defun eask-depth () (eask--str2num (eask--flag-value "--depth")))  ; --depth is enabled

(defun eask--handle-global-options ()
  "Handle global options."
  (when (eask-debug-p) (setq debug-on-error t))
  (eask--add-proxy "http"     (eask-proxy))
  (eask--add-proxy "https"    (eask-proxy))
  (eask--add-proxy "http"     (eask-http-proxy))
  (eask--add-proxy "https"    (eask-https-proxy))
  (eask--add-proxy "no_proxy" (eask-no-proxy)))

;;
;;; Proxy

(defun eask--add-proxy (protocal host)
  "Add proxy."
  (when host (push (cons protocal (eask-proxy)) url-proxy-services)))

;;
;;; Execution

(defconst eask--script (nth 1 (member "-scriptload" command-line-args))
  "Script currently executing.")

(defun eask-command ()
  "What's the current command?"
  (file-name-sans-extension (file-name-nondirectory eask--script)))

(defun eask-script (script)
  "Return full script filename."
  (let* ((script-el (concat script ".el"))
         (lisp-dir (file-name-directory eask--script))
         (script-file (expand-file-name script-el lisp-dir)))
    script-file))

(defvar eask-loading-file-p nil
  "This became t; if we are loading script from another file and not expecting
the `eask-start' execution.")

(defun eask-load (script)
  "Load another eask script; so we can reuse functions across all scripts."
  (let ((eask-loading-file-p t)) (eask-call script)))

(defun eask-call (script)
  "Call another eask script."
  (if-let* ((script-file (eask-script script))
            (_ (file-exists-p script-file)))
      (load script-file nil t)
    (error "Scripting missing %s..." script-file)))

;;
;;; Core

(defvar eask--first-init-p nil
  "Is non-nil if .eask does not exists; meaning users haven't called eask in the
current workspace.")

(defvar eask--initialized-p nil
  "Set to t once the environment setup has done; this is used when calling
other scripts internally.  See function `eask-call'.")

(defconst eask--command-list
  (mapcar (lambda (elm) (concat "--eask" elm))
          '("-g" "-f" "--depth" "--dev"
            "--proxy" "--http-proxy" "--https-proxy" "--no-proxy"
            "--debug" "--verbose" "--silent"))
  "List of commands to accept, so we can avoid unknown option error.")

(defun eask-self-command-p (arg)
  "Return non-nil if ARG is known internal command."
  (member arg eask--command-list))

(defun eask-argv (index) "Return argument value by INDEX." (elt argv index))

(defun eask-args ()
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

(defun eask-file-load (location &optional noerror)
  "Load Eask file in the LOCATION."
  (setq eask-file (expand-file-name location user-emacs-directory))
  (eask--alias-env (load eask-file noerror)))

(defmacro eask-start (&rest body)
  "Execute BODY with workspace setup."
  (declare (indent 0) (debug t))
  `(unless eask-loading-file-p
     (eask--setup-env
       (cond
        (eask--initialized-p ,@body)
        ((eask-global-p)
         (let ((eask--initialized-p t))
           (eask-pkg-init)
           (load (locate-user-emacs-file "early-init.el") t)
           (load (locate-user-emacs-file "../.emacs") t)
           (load (locate-user-emacs-file "init.el") t)
           ;; We accept Eask file in global scope, but it shouldn't be used
           ;; as a sandbox.
           (eask-file-load "./Eask" t)
           ,@body))
        (t
         (let* ((eask--initialized-p t)
                (user-emacs-directory (expand-file-name (concat ".eask/" emacs-version "/")))
                (package-user-dir (expand-file-name "elpa" user-emacs-directory))
                (eask--first-init-p (not (file-directory-p user-emacs-directory)))
                (user-init-file (locate-user-emacs-file "init.el"))
                (custom-file (locate-user-emacs-file "custom.el")))
           (eask--handle-global-options)
           (eask-file-load "../../Eask")
           (ignore-errors (make-directory package-user-dir t))
           (run-hooks 'eask-before-command-hook)
           (run-hooks (intern (concat "eask-before-command-" (eask-command) "-hook")))
           ,@body
           (run-hooks (intern (concat "eask-after-command-" (eask-command) "-hook")))
           (run-hooks 'eask-after-command-hook)))))))

;;
;;; Eask file

(defconst eask-source-mapping
  (let ((secure (if (gnutls-available-p) "s" "")))
    `((gnu          . ,(format "http%s://elpa.gnu.org/packages/"                   secure))
      (nongnu       . ,(format "http%s://elpa.nongnu.org/nongnu/"                  secure))
      (celpa        . ,(format "http%s://celpa.conao3.com/packages/"               secure))
      (jcs-elpa     . ,(format "http%s://jcs-emacs.github.io/jcs-elpa/packages/"   secure))
      (marmalade    . ,(format "http%s://marmalade-repo.org/packages/"             secure))
      (melpa        . ,(format "http%s://melpa.org/packages/"                      secure))
      (melpa-stable . ,(format "http%s://stable.melpa.org/packages/"               secure))
      (org          . ,(format "http%s://orgmode.org/elpa/"                        secure))
      (shmelpa      . ,(format "http%s://shmelpa.commandlinesystems.com/packages/" secure))))
  "Mapping of source name and url.")

(defvar eask-package        nil)
(defvar eask-package-file   nil)
(defvar eask-files          nil)
(defvar eask-depends-on     nil)
(defvar eask-depends-on-dev nil)

(defun eask-package-get (key)
  "Return package info by KEY."
  (plist-get eask-package key))

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
  (if (string= pkg "emacs")
      (when (and minimum-version (version< emacs-version minimum-version))
        (error "\n This requires Emacs %s and above!" minimum-version))
    (push pkg eask-depends-on)
    (delete-dups eask-depends-on))
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

(defun eask--trigger-error (&rest args)
  "Trigger error."
  ;; XXX Log out the error explicitly, so the user will know what causes Emacs
  ;; to crash.
  (message "Error: %s" (apply #'format-message args))
  (add-hook 'eask-after-command-hook #'eask--exit))

(advice-add 'error :before #'eask--trigger-error)

;;
;;; File

(eask-load "./extern/package-build")

(defun eask-guess-package-name ()
  "Return the possible package name."
  (or (eask-package-get :name)
      (file-name-nondirectory (file-name-sans-extension eask-package-file))))

(defun eask-package-files ()
  "Return package files in workspace."
  (let ((files (mapcar (lambda (elm) (expand-file-name (car elm) default-directory))
                       (package-build-expand-file-specs default-directory eask-files nil t))))
    ;; Package file is part of package-files
    (when eask-package-file (push eask-package-file files))
    (delete-dups files)
    (setq files (cl-remove-if-not #'file-exists-p files))
    (unless files
      (message "No matching file(s) found in %s: %s" default-directory eask-files))
    files))

(defun eask-package-el-files ()
  "Return package files in workspace."
  (cl-remove-if-not (lambda (filename) (string= (file-name-extension filename) "el")) (eask-package-files)))

(defun eask-package-elc-files ()
  "Return package files' elc in workspace."
  (when-let ((elcs (mapcar (lambda (elm) (concat elm "c")) (eask-package-el-files))))
    (setq elcs (cl-remove-if-not (lambda (elm) (file-exists-p elm)) elcs))
    elcs))

(defun eask-package-multi-p ()
  "Return t if the package is multi-files."
  (< 1 (length (eask-package-files))))

;;; _prepare.el ends here

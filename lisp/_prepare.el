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

;; Determine the underlying operating system
(defconst eask-is-windows (memq system-type '(cygwin windows-nt ms-dos))   "Windows")
(defconst eask-is-mac     (eq system-type 'darwin)                         "macOS")
(defconst eask-is-linux   (eq system-type 'gnu/linux)                      "Linux")
(defconst eask-is-bsd     (or eask-is-mac (eq system-type 'berkeley-unix)) "BSD")

(defconst eask-system-type
  (cond (eask-is-windows 'dos)
        (eask-is-bsd     'mac)
        (eask-is-linux   'unix)
        (t               'unknown))
  "Return current OS type.")

(setq make-backup-files nil)

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
;;; Util

(defmacro eask--silent (&rest body)
  "Execute BODY without message."
  (declare (indent 0) (debug t))
  `(let ((inhibit-message t) message-log-max) ,@body))

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

(defun eask-install-dependencies ()
  "Install dependencies defined in Eask file."
  ;;
  ;; XXX Without ignore-errors guard, it will trigger error
  ;;
  ;;   Can't find library xxxxxxx.el
  ;;
  ;; But we can remove this after Emacs 28, since function `find-library-name'
  ;; has replaced the function `signal' instead of the `error'.
  (eask-ignore-errors
    (mapc #'eask-package-install eask-depends-on)
    (when (eask-dev-p) (mapc #'eask-package-install eask-depends-on-dev))))

(defun eask-pkg-init ()
  "Package initialization."
  (eask-with-verbosity 'log
    (package-initialize)
    (package-refresh-contents))
  (eask--silent
    (eask--update-exec-path)
    (eask--update-load-path))
  (eask-install-dependencies))

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
(defun eask-global-p ()        (eask--flag "-g"))               ; -g, --global
(defun eask-force-p ()         (eask--flag "-f"))               ; -f, --force
(defun eask-dev-p ()           (eask--flag "--dev"))            ; --dev, --development
(defun eask-debug-p ()         (eask--flag "--debug"))          ; --debug
(defun eask-strict-p ()        (eask--flag "--strict"))         ; --strict
(defun eask-timestamps-p ()    (eask--flag "--timestamps"))     ; --timestamps
(defun eask-no-timestamps-p () (eask--flag "--no-timestamps"))  ; --no-timestamps
(defun eask-log-level-p ()     (eask--flag "--log-level"))      ; --log-level
(defun eask-no-log-level-p ()  (eask--flag "--no-log-level"))   ; --no-log-level

;;; String (with arguments)
(defun eask-proxy ()       (eask--flag-value "--proxy"))        ; --proxy
(defun eask-http-proxy ()  (eask--flag-value "--http-proxy"))   ; --http-proxy
(defun eask-https-proxy () (eask--flag-value "--https-proxy"))  ; --https-proxy
(defun eask-no-proxy ()    (eask--flag-value "--no-proxy"))     ; --no-proxy
(defun eask-destination () (eask--flag-value "--dest"))         ; --dest, --destintation
(defalias 'eask-dest #'eask-destination)

;;; Number (with arguments)
(defun eask-depth () (eask--str2num (eask--flag-value "--depth")))       ; --depth
(defun eask-verbose () (eask--str2num (eask--flag-value "--verbose")))   ; -v, --verbose

(defun eask--handle-global-options ()
  "Handle global options."
  (when (eask-debug-p) (setq debug-on-error t))
  (when (eask-verbose) (setq eask-verbosity (eask-verbose)))
  (when (eask-timestamps-p) (setq eask-timestamps t))
  (when (eask-no-timestamps-p) (setq eask-timestamps nil))
  (when (eask-log-level-p) (setq eask-log-level t))
  (when (eask-no-log-level-p) (setq eask-log-level nil))
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
            ((file-exists-p script-file)))
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

(defun eask--form-options (options)
  "Add --eask to all OPTIONS."
  (mapcar (lambda (elm) (concat "--eask" elm)) options))

(defconst eask--option-switches
  (eask--form-options
   '("-g" "-f" "--depth" "--dev"
     "--debug" "--strict"
     "--timestamps" "--no-timestamps"
     "--log-level" "--no-log-level"))
  "List of boolean type options")

(defconst eask--option-args
  (eask--form-options
   '("--proxy" "--http-proxy" "--https-proxy" "--no-proxy"
     "--verbose" "--silent"
     "--dest"))
  "List of arguments (number/string) type options.")

(defconst eask--command-list
  (append eask--option-switches eask--option-args)
  "List of commands to accept, so we can avoid unknown option error.")

(defun eask-self-command-p (arg)
  "Return non-nil if ARG is known internal command."
  (member arg eask--command-list))

(defun eask-argv (index) "Return argument value by INDEX." (elt argv index))

(defun eask-args ()
  "Get all arguments except options."
  (let ((argv (cl-remove-if (lambda (arg) (member arg eask--option-switches)) argv))
        (args) (skip-next))
    (dolist (arg argv)
      (if skip-next (setq skip-next nil)
        (if (member arg eask--option-args)
            (setq skip-next t)
          (push arg args))))
    (reverse args)))

(defmacro eask--setup-env (&rest body)
  "Execute BODY with workspace setup."
  (declare (indent 0) (debug t))
  `(let (alist)
     (dolist (cmd eask--command-list)
       (push (cons cmd '(lambda (&rest _))) alist))
     (setq command-switch-alist (append command-switch-alist alist))
     ,@body))

(defconst eask-file-keywords
  '("package" "package-file" "files"
    "depends-on" "development"
    "source" "source-priority"
    "exec-paths" "load-paths")
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
  `(let (result)
     ;; XXX magic here is we replace all keyword functions with `eask-xxx'...
     (eask--loop-file-keywords
      (lambda (keyword api old)
        (defalias old (symbol-function keyword))
        (defalias keyword (symbol-function api))))
     (setq result (progn ,@body))
     ;; XXX after loading Eask file, we revert those functions back to normal!
     (eask--loop-file-keywords
      (lambda (keyword api old)
        (defalias keyword (symbol-function old))))
     result))

(defvar eask-file nil "The Eask file path.")

(defun eask-file-load (location &optional noerror)
  "Load Eask file in the LOCATION."
  (when-let* ((target-eask-file (expand-file-name location user-emacs-directory))
              (result (eask--alias-env (load target-eask-file noerror t))))
    (setq eask-file target-eask-file)  ; assign eask file only if success
    result))

(defun eask--print-env-info ()
  "Display environment information at the very top of the execution."
  (message "")
  (message "✓ Checking Emacs version %s... done!" emacs-version)
  (message "✓ Checking system %s... done!" system-type))

(defun eask-file-try-load (relative-path)
  "Try load eask file in RELATIVE-PATH."
  (or (eask-file-load (concat relative-path "Easkfile") t)
      (eask-file-load (concat relative-path "Eask") t)))

(defmacro eask-start (&rest body)
  "Execute BODY with workspace setup."
  (declare (indent 0) (debug t))
  `(unless eask-loading-file-p
     (eask--setup-env
       (if eask--initialized-p (progn ,@body)
         (eask--handle-global-options)
         (eask--print-env-info)
         (setq eask--initialized-p t)
         (cond
          ((eask-global-p)
           (eask-pkg-init)
           (eask-with-verbosity 'debug
             (load (locate-user-emacs-file "early-init.el") t)
             (load (locate-user-emacs-file "../.emacs") t)
             (load (locate-user-emacs-file "init.el") t))
           ;; We accept Eask file in global scope, but it shouldn't be used
           ;; as a sandbox.
           (if (eask-file-try-load "./")
               (eask-debug "✓ Loading default Eask file in %s... done!" eask-file)
             (eask-warn "✗ Loading default Eask file... missing!"))
           (message "") ,@body)
          (t
           (let* ((user-emacs-directory (expand-file-name (concat ".eask/" emacs-version "/")))
                  (package-user-dir (expand-file-name "elpa" user-emacs-directory))
                  (eask--first-init-p (not (file-directory-p user-emacs-directory)))
                  (user-init-file (locate-user-emacs-file "init.el"))
                  (custom-file (locate-user-emacs-file "custom.el")))
             (if (eask-file-try-load "../../")
                 (eask-debug "✓ Loading Eask file in %s... done!" eask-file)
               (eask-error "✗ Loading Eask file... missing!"))
             (ignore-errors (make-directory package-user-dir t))
             (run-hooks 'eask-before-command-hook)
             (run-hooks (intern (concat "eask-before-command-" (eask-command) "-hook")))
             (message "") ,@body
             (run-hooks (intern (concat "eask-after-command-" (eask-command) "-hook")))
             (run-hooks 'eask-after-command-hook))))))))

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

(defun eask-depends-on (pkg &optional minimum-version &rest _rcp)
  "Specify a dependency of this package."
  (if (string= pkg "emacs")
      (when (and minimum-version (version< emacs-version minimum-version))
        (error "This requires Emacs %s and above!" minimum-version))
    (push pkg eask-depends-on)
    (delete-dups eask-depends-on))
  pkg)

(defun eask-development (&rest dep)
  "Development scope."
  (dolist (pkg dep)
    (push pkg eask-depends-on-dev)
    (delete-dups eask-depends-on-dev)
    (setq eask-depends-on (remove pkg eask-depends-on))))  ; remove it from production

(defun eask-load-paths (&rest dirs)
  "Add all DIRS to load-path."
  (dolist (dir dirs) (add-to-list 'load-path (expand-file-name dir))))

(defun eask-exec-paths (&rest dirs)
  "Add all DIRS to exec-path."
  (dolist (dir dirs) (add-to-list 'exec-path (expand-file-name dir))))

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

(defvar eask--ignore-error-p nil
  "Don't trigger error when this is non-nil.")

(defmacro eask-ignore-errors (&rest body)
  "Execute BODY but ignore all errors."
  (declare (indent 0) (debug t))
  `(let ((eask--ignore-error-p t)) ,@body))

(defun eask--exit (&rest _) "Send exit code." (kill-emacs 1))

(defun eask--trigger-error (&rest args)
  "Trigger error."
  (unless eask--ignore-error-p
    ;; XXX Log out the error explicitly, so the user will know what causes Emacs
    ;; to crash.
    (let ((eask-log-level t))
      (eask-error "%s" (apply #'format-message args)))
    (add-hook 'eask-after-command-hook #'eask--exit)))

(advice-add 'error :before #'eask--trigger-error)

;;
;;; Verbosity

(defcustom eask-verbosity 3
  "Log level for all messages; 4 means trace most anything, 0 means nothing.

Standard is, 0 (error), 1 (warning), 2 (info), 3 (log), 4 or above (debug)."
  :type 'integer
  :group 'eask)

(defcustom eask-timestamps nil
  "Log messages with timestamps."
  :type 'boolean
  :group 'eask)

(defcustom eask-log-level nil
  "Log messages with level."
  :type 'boolean
  :group 'eask)

(defun eask--verb2lvl (symbol)
  "Convert verbosity SYMBOL to level."
  (cl-case symbol
    (`debug 4)
    (`log   3)
    (`info  2)
    (`warn  1)
    (`error 0)
    (t symbol)))

(defmacro eask-with-verbosity (symbol &rest body)
  "If LEVEL is above `eask-verbosity'; hide all messages in BODY."
  (declare (indent 1) (debug t))
  `(if (>= eask-verbosity (eask--verb2lvl ,symbol)) (progn ,@body) (eask--silent ,@body)))

(defun eask-debug (msg &rest args) (apply #'eask--msg 'debug "[DEBUG]"   msg args))
(defun eask-log   (msg &rest args) (apply #'eask--msg 'log   "[LOG]"     msg args))
(defun eask-info  (msg &rest args) (apply #'eask--msg 'info  "[INFO]"    msg args))
(defun eask-warn  (msg &rest args) (apply #'eask--msg 'warn  "[WARNING]" msg args))
(defun eask-error (msg &rest args) (apply #'eask--msg 'error "[ERROR]"   msg args))

(defun eask--msg (level prefix msg &rest args)
  "If LEVEL is at or below `eask-verbosity', log message."
  (eask-with-verbosity level
    (message "%s" (apply #'eask--format prefix msg args))))

(defun eask--format (prefix format-control &rest format-args)
  "Format Eask messages."
  (apply #'format
         (concat (when eask-timestamps (format-time-string "%Y-%m-%d %H:%M:%S "))
                 (when eask-log-level (concat prefix " "))
                 format-control)
         format-args))

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
      (eask-debug "No matching file(s) found in %s: %s" default-directory eask-files))
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
  "Return t if multi-files package."
  (< 1 (length (eask-package-files))))

(defun eask-package-single-p ()
  "Return t if single file package."
  (not (eask-package-multi-p)))

;;
;;; Progress

(defun eask-progress (prefix sequence suffix func)
  "Progress SEQUENCE with messages."
  (let* ((total (length sequence)) (count 0)
         (offset (format "%s" (length (format "%s" total)))))
    (mapc
     (lambda (item)
       (cl-incf count)
       (funcall func item)
       (message (concat "  - %s [%" offset "d/%d] %s%s") prefix count total item suffix))
     sequence)))

;;
;;; User customization

(defcustom eask-dist-path "dist"
  "Name of default target directory for building packages."
  :type 'string
  :group 'eask)

;;; _prepare.el ends here

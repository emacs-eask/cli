;;; _prepare.el --- Prepare for command tasks  -*- lexical-binding: t; -*-
;;; Commentary: Prepare to setup Eask environment for sandboxing
;;; Code:

(require 'package)
(require 'project)
(require 'nsm)
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
;;; Util

(defmacro eask-defvc< (version &rest body)
  "Define scope if Emacs version is below VERSION."
  (declare (indent 1) (debug t))
  `(when (< emacs-major-version ,version) ,@body))

(defmacro eask--silent (&rest body)
  "Execute BODY without message."
  (declare (indent 0) (debug t))
  `(let ((inhibit-message t) message-log-max) ,@body))

(defmacro eask--unsilent (&rest body)
  "Execute BODY with message."
  (declare (indent 0) (debug t))
  `(let (inhibit-message) ,@body))

(defun eask--sinr (len-or-list form-1 form-2)
  "If LEN-OR-LIST has length of 1; return FORM-1, else FORM-2."
  (let ((len (if (numberp len-or-list) len-or-list (length len-or-list))))
    (if (= 1 len) form-1 form-2)))

(defun eask-seq-str-max (sequence)
  "Return max length in list of strings."
  (let ((result 0))
    (mapc (lambda (elm) (setq result (max result (length (format "%s" elm))))) sequence)
    result))

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

(defun eask-dependencies ()
  "Return list of dependencies."
  (append eask-depends-on (and (eask-dev-p) eask-depends-on-dev)))

(defun eask--extract-deps-name (dependencies)
  "Use `car' to get all names from DEPENDENCIES."
  (mapcar (lambda (dep) (format "%s" (car dep))) dependencies))

(defun eask--install-deps (dependencies msg)
  "Install DEPENDENCIES."
  (let* ((names (eask--extract-deps-name dependencies))
         (len (length dependencies))
         (ies (eask--sinr len "y" "ies"))
         (pkg-installed (cl-remove-if #'package-installed-p names))
         (installed (length pkg-installed)) (skipped (- len installed)))
    (eask-log "Installing %s %s dependenc%s..." len msg ies)
    (mapc #'eask-package-install names)
    (eask-info "(Total of %s dependenc%s installed, %s skipped)"
               installed ies skipped)))

(defun eask-install-dependencies ()
  "Install dependencies defined in Eask file."
  (eask-pkg-init)
  (when eask-depends-on-recipe-p
    (eask-log "Installing required external packages...")
    (eask-package-install 'package-build)
    (eask-with-progress
      "Building temporary archives (this may take awhile)... "
      (eask-with-verbosity 'debug (github-elpa-build))
      "done ✓")
    (eask-with-progress
      "Adding local archives... "
      (eask-with-verbosity 'debug
        (setq package-archives
              `(,@package-archives ("local" . ,github-elpa-archive-dir))
              ;; If the local archives is added, we set the priority to a very
              ;; high number so user we always use the specified dependencies!
              package-archive-priorities
              `(,@package-archive-priorities ("local" . 90)))
        (eask-call "archives"))
      "done ✓"))
  (when eask-depends-on
    (eask--install-deps eask-depends-on "package"))
  (when (and eask-depends-on-dev (eask-dev-p))
    (eask--install-deps eask-depends-on-dev "development")))

(defun eask-setup-paths ()
  "Setup both `exec-path' and `load-path'."
  (eask-with-progress
    (ansi-green "Updating environment PATHs... ")
    (eask-with-verbosity 'debug
      (eask--update-exec-path) (eask--update-load-path))
    (ansi-green "done ✓")))

(defun eask-pkg-init ()
  "Package initialization."
  (unless (or package--initialized package-archive-contents)
    (eask-with-progress
      (ansi-green "Loading package information... ")
      (eask-with-verbosity 'debug
        (package--archives-initialize))
      (ansi-green "done ✓"))))

(defun eask--pkg-transaction-vars (pkg)
  "Return 1 symbol and 2 strings."
  (let* (;; Ensure symbol
         (pkg (if (stringp pkg) (intern pkg) pkg))
         ;; Wrap package name with color
         (pkg-string (ansi-green (format "%s" pkg)))
         ;; Wrap version number with color
         (pkg-version (ansi-yellow (eask-package--version-string pkg))))
    (list pkg pkg-string pkg-version)))

(defun eask-package-install (pkg)
  "Install the package."
  (eask-pkg-init)
  (let* ((pkg-info (eask--pkg-transaction-vars pkg))
         (pkg         (nth 0 pkg-info))
         (pkg-string  (nth 1 pkg-info))
         (pkg-version (nth 2 pkg-info)))
    (if (package-installed-p pkg)
        (eask-msg "  - Skipping %s (%s)... already installed ✗" pkg-string pkg-version)
      (eask-with-progress
        (format "  - Installing %s (%s)... " pkg-string pkg-version)
        (eask-with-verbosity 'debug
          ;; XXX Without ignore-errors guard, it will trigger error
          ;;
          ;;   Can't find library xxxxxxx.el
          ;;
          ;; But we can remove this after Emacs 28, since function `find-library-name'
          ;; has replaced the function `signal' instead of the `error'.
          (eask-ignore-errors (package-install pkg)))
        "done ✓"))))

(defun eask-package-delete (pkg)
  "Delete the package."
  (eask-pkg-init)
  (let* ((pkg-info (eask--pkg-transaction-vars pkg))
         (pkg         (nth 0 pkg-info))
         (pkg-string  (nth 1 pkg-info))
         (pkg-version (nth 2 pkg-info)))
    (if (not (package-installed-p pkg))
        (eask-msg "  - Skipping %s (%s)... not installed ✗" pkg-string pkg-version)
      (eask-with-progress
        (format "  - Uninstalling %s (%s)... " pkg-string pkg-version)
        (eask-with-verbosity 'debug
          (package-delete (eask-package-desc pkg t) (eask-force-p)))
        "done ✓"))))

(defun eask-package-desc (name &optional current)
  "Build package description by PKG-NAME."
  (cadr (assq name (if current package-alist
                     (or package-archive-contents package-alist)))))

(defun eask-package--version (name &optional current)
  "Return PKG's version."
  (when-let ((desc (eask-package-desc name current)))
    (package-desc-version desc)))

(defun eask-package--version-string (pkg)
  "Return PKG's version."
  (if-let ((version (eask-package--version pkg)))
      (package-version-join version)
    "latest"))

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
(defun eask-no-color-p ()      (eask--flag "--no-color"))       ; --no-color
(defun eask-allow-error-p ()   (eask--flag "--allow-error"))    ; --allow-error
(defun eask-insecure-p ()      (eask--flag "--insecure"))       ; --insecure

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
  (when (eask-insecure-p) (setq network-security-level 'low))
  (when (eask-timestamps-p) (setq eask-timestamps t))
  (when (eask-no-timestamps-p) (setq eask-timestamps nil))
  (when (eask-log-level-p) (setq eask-log-level t))
  (when (eask-no-log-level-p) (setq eask-log-level nil))
  (when (eask-no-color-p) (setq ansi-inhibit-ansi t))
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
     "--log-level" "--no-log-level"
     "--no-color"
     "--allow-error"
     "--insecure"))
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

(defvar eask-file nil "The Eask file's filename.")
(defvar eask-file-root nil "The Eask file's directory .")

(defun eask-root-del (filename)
  "Remove Eask file root path from FILENAME."
  (s-replace eask-file-root "" filename))

(defun eask-file-load (location &optional noerror)
  "Load Eask file in the LOCATION."
  (when-let* ((target-eask-file (expand-file-name location user-emacs-directory))
              (result (eask--alias-env (load target-eask-file noerror t))))
    (setq eask-file target-eask-file  ; assign eask file only if success
          eask-file-root (file-name-directory target-eask-file))
    (run-hooks 'eask-file-loaded-hook)
    result))

(defun eask--print-env-info ()
  "Display environment information at the very top of the execution."
  (eask-msg "")
  (eask-msg "✓ Checking Emacs version %s... done!" emacs-version)
  (eask-msg "✓ Checking system %s... done!" system-type))

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
           ;; We accept Eask file in global scope, but it shouldn't be used
           ;; as a sandbox.
           (if (eask-file-try-load "./")
               (eask-msg "✓ Loading default Eask file in %s... done!" eask-file)
             (eask-msg "✗ Loading default Eask file... missing!"))
           (message "")
           (package-activate-all)
           (eask-with-progress
             (ansi-green "Loading your configuration... ")
             (eask-with-verbosity 'debug
               (load (locate-user-emacs-file "early-init.el") t)
               (load (locate-user-emacs-file "../.emacs") t)
               (load (locate-user-emacs-file "init.el") t))
             (ansi-green "done"))
           ,@body)
          (t
           (let* ((user-emacs-directory (expand-file-name (concat ".eask/" emacs-version "/")))
                  (package-user-dir (expand-file-name "elpa" user-emacs-directory))
                  (eask--first-init-p (not (file-directory-p user-emacs-directory)))
                  (user-init-file (locate-user-emacs-file "init.el"))
                  (custom-file (locate-user-emacs-file "custom.el")))
             (if (eask-file-try-load "../../")
                 (eask-msg "✓ Loading Eask file in %s... done!" eask-file)
               (eask-msg "✗ Loading Eask file... missing!"))
             (message "")
             (ignore-errors (make-directory package-user-dir t))
             (eask--silent (eask-setup-paths))
             (run-hooks 'eask-before-command-hook)
             (run-hooks (intern (concat "eask-before-command-" (eask-command) "-hook")))
             ,@body
             (run-hooks (intern (concat "eask-after-command-" (eask-command) "-hook")))
             (run-hooks 'eask-after-command-hook))))))))

;;
;;; Eask file

(defun eask-network-insecure-p ()
  "Are we attempt to use insecure connection?"
  (eq network-security-level 'low))

(defconst eask-source-mapping
  (let* ((secure (and (gnutls-available-p)
                      (not (eask-network-insecure-p))))
         (proto (if secure "s" "")))
    `((gnu          . ,(format "http%s://elpa.gnu.org/packages/"                   proto))
      (nongnu       . ,(format "http%s://elpa.nongnu.org/nongnu/"                  proto))
      (celpa        . ,(format "http%s://celpa.conao3.com/packages/"               proto))
      (jcs-elpa     . ,(format "http%s://jcs-emacs.github.io/jcs-elpa/packages/"   proto))
      (marmalade    . ,(format "http%s://marmalade-repo.org/packages/"             proto))
      (melpa        . ,(format "http%s://melpa.org/packages/"                      proto))
      (melpa-stable . ,(format "http%s://stable.melpa.org/packages/"               proto))
      (org          . ,(format "http%s://orgmode.org/elpa/"                        proto))
      (shmelpa      . ,(format "http%s://shmelpa.commandlinesystems.com/packages/" proto))))
  "Mapping of source name and url.")

(defvar eask-package        nil)
(defvar eask-package-file   nil)
(defvar eask-files          nil)
(defvar eask-depends-on     nil)
(defvar eask-depends-on-dev nil)

(defun eask-package--get (key)
  "Return package info by KEY."
  (plist-get eask-package key))

(defun eask-package-name () (eask-package--get :name))
(defun eask-package-version () (eask-package--get :version))
(defun eask-package-description () (eask-package--get :description))

(defun eask-package (name version description)
  "Set the package information."
  (setq eask-package `(:name ,name :version ,version :description ,description)))

(defun eask-package-file (file)
  "Set package file."
  (if eask-package-file
      (error "Multiple package-file detected, please specify one unique pacakge-file")
    (setq eask-package-file (expand-file-name file))))

(defun eask-files (&rest patterns)
  "Set files patterns."
  (setq eask-files patterns))

(defvar eask-depends-on-recipe-p nil
  "Set to t if package depends on recipe.")

(add-hook 'eask-file-loaded-hook
          (lambda ()
            (setq eask-depends-on (reverse eask-depends-on)
                  eask-depends-on-dev (reverse eask-depends-on-dev))))

(defun eask-depends-on (pkg &rest args)
  "Specify a dependency of this package."
  (cond
   ((string= pkg "emacs")
    (let ((minimum-version (car args)))
      (when (version< emacs-version minimum-version)
        (error "This requires Emacs %s and above!" minimum-version)))
    pkg)
   ;; No argument specify
   ((<= (length args) 1)
    (let* ((minimum-version (or (car args) "latest"))
           (recipe (list pkg minimum-version)))
      (if (member pkg eask-depends-on)
          (error "Duplicate dependencies with name: %s" pkg)
        (push recipe eask-depends-on))
      recipe))
   ;; recipe are entered
   (t
    (let ((recipe (append (list (intern pkg)) args)))
      (if (member recipe eask-depends-on)
          (error "Duplicate dependencies with name: %s" pkg)
        (push recipe eask-depends-on)
        (eask-load "./extern/github-elpa")
        (write-region (pp-to-string recipe) nil (expand-file-name pkg github-elpa-recipes-dir))
        (setq eask-depends-on-recipe-p t))
      recipe))))

(defun eask-development (&rest dep)
  "Development scope."
  (dolist (pkg dep)
    (push pkg eask-depends-on-dev)
    (delete-dups eask-depends-on-dev)
    (setq eask-depends-on (remove pkg eask-depends-on))))

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

(defun eask--trigger-error ()
  "Trigger error at the right time."
  (unless eask--ignore-error-p
    (if (eask-allow-error-p)
        (add-hook 'eask-after-command-hook #'eask--exit)
      (eask--exit))))

(defun eask--error (fnc &rest args)
  "On error."
  (eask--unsilent (eask-msg "%s" (eask--ansi 'error (apply #'format-message args))))
  (eask--trigger-error)
  (when debug-on-error (apply fnc args)))

(advice-add 'error :around #'eask--error)

(defun eask--warn (&rest args)
  "On warn."
  (eask--unsilent
    (eask-msg "%s" (eask--ansi 'warn (apply #'format-message args)))))

(advice-add 'warn :override #'eask--warn)

;;
;;; Verbosity

(defcustom eask-verbosity 3
  "Log level for all messages; 4 means trace most anything, 0 means nothing.

Standard is, 0 (error), 1 (warning), 2 (info), 3 (log), 4 or above (debug)."
  :type 'integer)

(defcustom eask-timestamps nil
  "Log messages with timestamps."
  :type 'boolean)

(defcustom eask-log-level nil
  "Log messages with level."
  :type 'boolean)

(defcustom eask-level-color
  '((debug . ansi-blue)
    (log   . ansi-white)
    (info  . ansi-cyan)
    (warn  . ansi-yellow)
    (error . ansi-red))
  "Alist of each log level's color, in (SYMBOL . ANSI-FUNCTION)."
  :type 'alist)

(defun eask--verb2lvl (symbol)
  "Convert verbosity SYMBOL to level."
  (cl-case symbol
    (`debug 4)
    (`log   3)
    (`info  2)
    (`warn  1)
    (`error 0)
    (t symbol)))

(defun eask--reach-verbosity-p (symbol)
  "Return t if SYMBOL reach verbosity (should be printed)."
  (>= eask-verbosity (eask--verb2lvl symbol)))

(defmacro eask-with-verbosity (symbol &rest body)
  "If LEVEL is above `eask-verbosity'; hide all messages in BODY."
  (declare (indent 1) (debug t))
  `(if (eask--reach-verbosity-p ,symbol) (progn ,@body)
     (eask--silent ,@body)))

(defun eask-debug (msg &rest args) (apply #'eask--msg 'debug "[DEBUG]"   msg args))
(defun eask-log   (msg &rest args) (apply #'eask--msg 'log   "[LOG]"     msg args))
(defun eask-info  (msg &rest args) (apply #'eask--msg 'info  "[INFO]"    msg args))
(defun eask-warn  (msg &rest args) (apply #'eask--msg 'warn  "[WARNING]" msg args))
(defun eask-error (msg &rest args)
  (apply #'eask--msg 'error "[ERROR]"   msg args)
  (eask--trigger-error))

(defun eask--ansi (symbol string)
  "Paint STRING with color defined by log level."
  (if-let ((ansi-function (cdr (assq symbol eask-level-color))))
      (funcall ansi-function string)
    string))

(defun eask--msg (symbol prefix msg &rest args)
  "If LEVEL is at or below `eask-verbosity', log message."
  (eask-with-verbosity symbol
    (let* ((string (apply #'eask--format prefix msg args))
           (output (eask--ansi symbol string)))
      (message "%s" output))))

(defun eask--format (prefix fmt &rest args)
  "Format Eask messages."
  (apply #'format
         (concat (when eask-timestamps (format-time-string "%Y-%m-%d %H:%M:%S "))
                 (when eask-log-level (concat prefix " "))
                 fmt)
         args))

(defun eask--msg-paint-kwds (string)
  "Paint keywords from STRING."
  (let* ((string (s-replace "✓" (ansi-green "✓") string))
         (string (s-replace "✗" (ansi-red "✗") string)))
    string))

(defun eask--format-paint-kwds (msg &rest args)
  "Paint keywords after format MSG and ARGS."
  (let* ((string (apply #'format msg args))
         (string (eask--msg-paint-kwds string)))
    string))

(defun eask-msg (msg &rest args)
  "Like function `message' but replace unicodes with color."
  (message (apply #'eask--format-paint-kwds msg args)))

(defun eask-write (msg &rest args)
  "Like function `eask-msg' but without newline at the end."
  (unless inhibit-message
    (princ (apply #'eask--format-paint-kwds msg args) 'external-debugging-output)))

;;
;;; File

(defun eask-guess-package-name ()
  "Return the possible package name."
  (or (eask-package-name)
      (ignore-errors (file-name-nondirectory
                      (file-name-sans-extension eask-package-file)))))

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

(defmacro eask-with-progress (msg-start body msg-end)
  "Progress BODY wrapper with prefix and suffix messages."
  (declare (indent 0) (debug t))
  `(progn (ignore-errors (eask-write ,msg-start)) ,body
          (ignore-errors (eask-msg ,msg-end))))

(defun eask-progress-seq (prefix sequence suffix func)
  "Progress SEQUENCE with messages."
  (let* ((total (length sequence)) (count 0)
         (offset (format "%s" (length (format "%s" total)))))
    (mapc
     (lambda (item)
       (cl-incf count)

       (eask-with-progress
         (format (concat "%s [%" offset "d/%d] %s... ") prefix count total
                 (ansi-green "%s" item))
         (when func (funcall func item))
         suffix))
     sequence)))

(defun eask-print-log-buffer (&optional buffer-or-name)
  "Loop through each line and print each line with corresponds log level."
  (with-current-buffer (or buffer-or-name (current-buffer))
    (goto-char (point-min))
    (while (not (eobp))
      (let ((line (buffer-substring-no-properties (line-beginning-position) (line-end-position))))
        (cond ((string-match-p " [Ee]rror: " line) (eask-error line))
              ((string-match-p " [Ww]arning: " line) (eask-warn line))
              (t (eask-log line))))
      (forward-line 1))))

;;
;;; User customization

(defcustom eask-dist-path "dist"
  "Name of default target directory for building packages."
  :type 'string)

(defcustom eask-before-command-hook nil
  "Hook runs before command is executed."
  :type 'hook)

(defcustom eask-after-command-hook nil
  "Hook runs after command is executed."
  :type 'hook)

(defcustom eask-file-loaded-hook nil
  "Hook runs after Easkfile is loaded."
  :type 'hook)

;;
;;; Externals

(eask-load "./extern/ansi")
(eask-load "./extern/package")
(eask-load "./extern/package-build")
(eask-load "./extern/s")

;;; _prepare.el ends here

;;; _prepare.el --- Prepare for command tasks  -*- lexical-binding: t; -*-
;;; Commentary: Prepare to setup Eask environment for sandboxing
;;; Code:

(require 'ansi-color)
(require 'package)
(require 'project)
(require 'nsm)
(require 'url-vars)

(require 'cl-lib)
(require 'ls-lisp)
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

(setq package-enable-at-startup  nil            ; To avoid initializing twice
      package-check-signature    nil
      package-archives           nil            ; Leave it to custom use
      package-archive-priorities nil)

(defun eask--load--adv (fnc &rest args)
  "Prevent `_prepare.el' loading twice."
  (unless (string= (nth 0 args) (eask-script "_prepare")) (apply fnc args)))
(advice-add 'load :around #'eask--load--adv)

;;
;;; Execution

(defconst eask-argv argv
  "This stores the real argv; the argv will soon be replaced with `(eask-args)'.")

(defconst eask--script (nth 1 (or (member "-scriptload" command-line-args)
                                  (member "-l" command-line-args)))
  "Script currently executing.")

(defconst eask-lisp-root
  (expand-file-name
   (concat (file-name-directory eask--script) "../"))
  "Source lisp directory; should always end with slash.")

(defun eask-command ()
  "What's the current command?

If the command is with subcommand, it will return command with concatenate with
dash separator. For example, the following:

   $ eask lint checkdoc [FILES..]

will return `lint-checkdoc' with a dash between two subcommands."
  (let* ((script-dir (file-name-directory eask--script))
         (script-file (file-name-sans-extension (file-name-nondirectory eask--script)))
         (module-name (s-replace eask-lisp-root "" script-dir))
         (module-name (s-replace "/" "" module-name)))
    ;; Ignore if it's inside core module
    (if (member module-name '("core" "checker")) script-file
      (concat module-name "-" script-file))))

(defun eask-special-p ()
  "Return t if the command that can be run without Eask-file existence."
  (member (eask-command) '("keywords")))

(defun eask-checker-p ()
  "Return t if running Eask as the checker."
  (member (eask-command) '("check-eask")))

(defun eask-script (script)
  "Return full script filename."
  (concat eask-lisp-root script ".el"))

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
    (eask-error "Scripting missing %s..." script-file)))

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

(defun eask-2str (obj)
  "Convert OBJ to string."
  (format "%s" obj))

(defun eask-listify (obj)
  "Turn OBJ to list."
  (if (listp obj) obj (list obj)))

(defun eask-intern (obj)
  "Safely intern OBJ."
  (if (stringp obj) (intern obj) obj))

(defun eask--sinr (len-or-list form-1 form-2)
  "If LEN-OR-LIST has length of 1; return FORM-1, else FORM-2."
  (let ((len (if (numberp len-or-list) len-or-list (length len-or-list))))
    (if (= 1 len) form-1 form-2)))

(defun eask-seq-str-max (sequence)
  "Return max length in list of strings."
  (let ((result 0))
    (mapc (lambda (elm) (setq result (max result (length (eask-2str elm))))) sequence)
    result))

;;
;;; Archive

(defun eask--download-archives ()
  "If archives download failed; download it manually."
  (dolist (archive package-archives)
    (let* ((location (cdr archive))
           (name (car archive))
           (file "archive-contents")
           (dir (expand-file-name (concat "archives/" name) package-user-dir))
           (local-file (expand-file-name file dir))
           (url (format
                 "https://raw.githubusercontent.com/emacs-eask/archives/master/%s/%s" name file))
           (download-p)
           (local-archive-p (string= name "local")))  ; exclude local elpa
      (unless (file-exists-p local-file)
        (eask-with-progress
          (format "Downloading archive `%s' manually... " (ansi-yellow name))
          (unless local-archive-p
            (if (url-file-exists-p url)
                (progn
                  (ignore-errors (make-directory dir t))
                  (url-copy-file url local-file t)
                  (setq download-p t))
              (eask-debug "No archive-contents found in `%s'" (ansi-yellow name))))
          (cond (download-p      "done ✓")
                (local-archive-p "skipped ✗")
                (t               "failed ✗"))))
      (when download-p (eask-pkg-init t)))))

;;
;;; Package

(defun eask--update-exec-path ()
  "Add all bin directory to `exec-path'."
  (dolist (filename (directory-files-recursively package-user-dir "^\\([^.]\\|\\.[^.]\\|\\.\\..\\)"))
    (when (string-suffix-p "bin/" (file-name-directory filename))
      (add-to-list 'exec-path (file-name-directory filename) t)))
  (delete-dups exec-path))

(defun eask--update-load-path ()
  "Add all load-path for all .el files."
  (dolist (filename (eask-package-el-files))
    (add-to-list 'load-path (file-name-directory filename) t))
  (delete-dups load-path))

(defun eask-dependencies ()
  "Return list of dependencies."
  (append eask-depends-on (and (eask-dev-p) eask-depends-on-dev)))

(defun eask--install-deps (dependencies msg)
  "Install DEPENDENCIES."
  (let* ((names (mapcar #'car dependencies))
         (names (mapcar #'eask-intern names))
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
  (eask-defvc< 27 (eask-pkg-init))  ; XXX: remove this after we drop 26.x
  (when eask-depends-on-recipe-p
    (eask-log "Installing required external packages...")
    (eask-with-archives "melpa"
      (eask-package-install 'package-build))
    (eask-with-progress
      "Building temporary archives (this may take a while)... "
      (eask-with-verbosity 'debug (github-elpa-build))
      "done ✓")
    (eask-pkg-init t))
  (when eask-depends-on
    (eask--install-deps eask-depends-on "package"))
  (when (and eask-depends-on-dev (eask-dev-p))
    (eask--install-deps eask-depends-on-dev "development")))

(defun eask-setup-paths ()
  "Setup both `exec-path' and `load-path'."
  (eask-with-progress
    (ansi-green "Updating environment variables... ")
    (eask-with-verbosity 'debug
      (eask--update-exec-path) (eask--update-load-path)
      (setenv "PATH" (string-join exec-path path-separator))
      (setenv "EMACSLOADPATH" (string-join load-path path-separator)))
    (ansi-green "done ✓")))

(defvar eask--package-initialized nil
  "Flag for package initialization in global scope.")

(defun eask-pkg-init (&optional force)
  "Package initialization."
  (when (or (not package--initialized) (not package-archive-contents) force
            ;; XXX we need to initialize once in global scope since most Emacs
            ;; configuration would likely to set `package-archives' variable
            ;; themselves.
            (and (eask-global-p) (not eask--package-initialized)))
    (setq eask--package-initialized t)
    (eask-with-progress
      (ansi-green "Loading package information... ")
      (eask-with-verbosity 'debug
        (package-initialize t) (package-refresh-contents)
        (eask--download-archives))
      (ansi-green "done ✓"))))

(defun eask--pkg-transaction-vars (pkg)
  "Return 1 symbol and 2 strings."
  (let* (;; Ensure symbol
         (pkg (if (stringp pkg) (intern pkg) pkg))
         ;; Wrap package name with color
         (pkg-string (ansi-green pkg))
         ;; Wrap version number with color
         (pkg-version (ansi-yellow (eask-package--version-string pkg))))
    (list pkg pkg-string pkg-version)))

(defmacro eask--pkg-process (pkg &rest body)
  "Execute BODY with PKG's related variables."
  (declare (indent 1) (debug t))
  `(let* ((pkg-info (eask--pkg-transaction-vars ,pkg))
          (pkg      (nth 0 pkg-info))
          (name     (nth 1 pkg-info))
          (version  (nth 2 pkg-info)))
     ,@body))

(defmacro eask-with-archives (archives &rest body)
  "Scope that temporary makes ARCHIVES available."
  (declare (indent 1) (debug t))
  `(let ((package-archives package-archives)
         (archives (eask-listify ,archives))
         (added))
     (dolist (archive archives)
       (unless (assoc archive package-archives)
         (setq added t)
         (eask-with-progress
           (format "Adding required archives (%s)... " (ansi-yellow archive))
           (eask-source archive)
           "done ✓")))
     (when added
       (eask-with-progress
         "Refresh archives information... "
         (eask--silent (eask-pkg-init t))
         "done ✓"))
     ,@body))

(defun eask-package-installable-p (pkg)
  "Return non-nil if package is installable."
  (assq (if (stringp pkg) (intern pkg) pkg) package-archive-contents))

(defun eask-package-install (pkg)
  "Install the package."
  (eask-defvc< 27 (eask-pkg-init))  ; XXX: remove this after we drop 26.x
  (eask--pkg-process pkg
    (cond
     ((package-installed-p pkg)
      (eask-msg "  - Skipping %s (%s)... already installed ✗" name version))
     ((progn
        (eask-pkg-init)
        (unless (eask-package-installable-p pkg)
          (eask-error "Package not installable `%s'; make sure package archive is included" pkg))))
     ((when-let* ((desc (eask-package-desc pkg))
                  (req-emacs (assoc 'emacs (package-desc-reqs desc)))
                  (req-emacs (package-version-join (nth 0 (cdr req-emacs))))
                  ((version< emacs-version req-emacs)))
        (if (eask-strict-p)
            (eask-error "  - Skipping %s (%s)... it requires Emacs %s and above ✗"
                        pkg (eask-package--version-string pkg) emacs-version)
          (eask-msg "  - Skipping %s (%s)... it requires Emacs %s and above ✗"
                    name version (ansi-yellow emacs-version)))))
     (t
      (eask--pkg-process pkg
        (eask-with-progress
          (format "  - Installing %s (%s)... " name version)
          (eask-with-verbosity 'debug
            ;; XXX Without ignore-errors guard, it will trigger error
            ;;
            ;;   Can't find library xxxxxxx.el
            ;;
            ;; But we can remove this after Emacs 28, since function `find-library-name'
            ;; has replaced the function `signal' instead of the `error'.
            (eask-ignore-errors (package-install pkg)))
          "done ✓"))))))

(defun eask-package-delete (pkg)
  "Delete the package."
  (eask-defvc< 27 (eask-pkg-init))  ; XXX: remove this after we drop 26.x
  (eask--pkg-process pkg
    (cond
     ((not (package-installed-p pkg))
      (eask-msg "  - Skipping %s (%s)... not installed ✗" name version))
     (t
      (eask--pkg-process pkg
        (eask-with-progress
          (format "  - Uninstalling %s (%s)... " name version)
          (eask-with-verbosity 'debug
            (package-delete (eask-package-desc pkg t) (eask-force-p)))
          "done ✓"))))))

(defun eask-package-reinstall (pkg)
  "Reinstall the package."
  (eask-defvc< 27 (eask-pkg-init))  ; XXX: remove this after we drop 26.x
  (eask--pkg-process pkg
    (cond
     ((not (package-installed-p pkg))
      (eask-msg "  - Skipping %s (%s)... not installed ✗" name version))
     (t
      (eask-pkg-init)
      (eask--pkg-process pkg
        (eask-with-progress
          (format "  - Reinstalling %s (%s)... " name version)
          (eask-with-verbosity 'debug
            (package-delete (eask-package-desc pkg t) t)
            (eask-ignore-errors (package-install pkg)))
          "done ✓"))))))

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
    ;; Just in case, but this should never happens!
    "latest"))

(defun eask-package-desc-url ()
  "Return url from package descriptor."
  (when eask-package-desc
    (when-let ((extras (package-desc-extras eask-package-desc)))
      (cdr (assoc :url extras)))))

(defun eask-package-desc-keywords ()
  "Return keywords from package descriptor."
  (when eask-package-desc (package-desc--keywords eask-package-desc)))

(defun eask-pkg-el ()
  "Return package description file if exists."
  (let ((pkg-el (package--description-file default-directory)))
    (when (file-readable-p pkg-el) pkg-el)))

;;
;;; Environments

(defconst eask-has-colors (getenv "EASK_HASCOLORS")
  "Return non-nil if terminal support colors.")

;;
;;; Flags

(defun eask--str2num (str) (ignore-errors (string-to-number str)))

(defun eask--flag (flag)
  "Return non-nil if FLAG exists.."
  (member (concat "--eask" flag) eask-argv))

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
(defun eask-log-level-p ()     (eask--flag "--log-level"))      ; --log-level
(defun eask-log-file-p ()      (eask--flag "--log-file"))       ; --log-file, --lf
(defun eask-elapsed-time-p ()  (eask--flag "--elapsed-time"))   ; --elapsed-time, --et
(defun eask-allow-error-p ()   (eask--flag "--allow-error"))    ; --allow-error
(defun eask-insecure-p ()      (eask--flag "--insecure"))       ; --insecure
(defun eask-no-color-p ()      (eask--flag "--no-color"))       ; --no-color

;;; String (with arguments)
(defun eask-proxy ()       (eask--flag-value "--proxy"))        ; --proxy
(defun eask-http-proxy ()  (eask--flag-value "--http-proxy"))   ; --http-proxy
(defun eask-https-proxy () (eask--flag-value "--https-proxy"))  ; --https-proxy
(defun eask-no-proxy ()    (eask--flag-value "--no-proxy"))     ; --no-proxy
(defun eask-destination () (eask--flag-value "--dest"))         ; --dest, --destination
(defalias 'eask-dest #'eask-destination)

;;; Number (with arguments)
(defun eask-depth () (eask--str2num (eask--flag-value "--depth")))       ; --depth
(defun eask-verbose () (eask--str2num (eask--flag-value "--verbose")))   ; -v, --verbose

(defun eask--handle-global-options ()
  "Handle global options."
  (when (eask-debug-p)        (setq debug-on-error t))
  (when (eask-verbose)        (setq eask-verbosity (eask-verbose)))
  (when (eask-insecure-p)     (setq network-security-level 'low))
  (when (eask-timestamps-p)   (setq eask-timestamps t))
  (when (eask-log-level-p)    (setq eask-log-level t))
  (when (eask-log-file-p)     (setq eask-log-file t))
  (when (eask-elapsed-time-p) (setq eask-elapsed-time t))
  (when (eask-no-color-p)     (setq ansi-inhibit-ansi t))
  (unless eask-has-colors     (setq ansi-inhibit-ansi t))
  (when (display-graphic-p)   (setq ansi-inhibit-ansi t))
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
   '("-g" "-f" "--dev"
     "--debug" "--strict"
     "--allow-error"
     "--insecure"
     "--timestamps" "--log-level"
     "--log-file"
     "--elapsed-time"
     "--no-color"))
  "List of boolean type options.")

(defconst eask--option-args
  (eask--form-options
   '("--proxy" "--http-proxy" "--https-proxy" "--no-proxy"
     "--verbose" "--silent"
     "--depth" "--dest"))
  "List of arguments (number/string) type options.")

(defconst eask--command-list
  (append eask--option-switches eask--option-args)
  "List of commands to accept, so we can avoid unknown option error.")

(defun eask-self-command-p (arg)
  "Return non-nil if ARG is known internal command."
  (member arg eask--command-list))

(defun eask-argv (index) "Return argument value by INDEX." (elt eask-argv index))

(defun eask-args ()
  "Get all arguments except options."
  (let ((argv (cl-remove-if (lambda (arg) (member arg eask--option-switches)) eask-argv))
        (args) (skip-next))
    (dolist (arg argv)
      (if skip-next (setq skip-next nil)
        (if (member arg eask--option-args)
            (setq skip-next t)
          (push arg args))))
    (reverse args)))

(defmacro eask--batch-mode (&rest body)
  "Initialize for batch-mode"
  (declare (indent 0) (debug t))
  `(let ((argv (eask-args))
         load-file-name buffer-file-name)
     ,@body))

(defmacro eask--setup-env (&rest body)
  "Execute BODY with workspace setup."
  (declare (indent 0) (debug t))
  `(eask--batch-mode
     (let (;; XXX: this will make command `info', `files' work as expected;
           ;; but the relative paths file spec will be lost...
           ;;
           ;; So commands like `load' would NOT work!
           (default-directory (if (eask-global-p) user-emacs-directory
                                default-directory))
           (alist))
       (dolist (cmd eask--command-list)
         (push (cons cmd '(lambda (&rest _))) alist))
       (setq command-switch-alist (append command-switch-alist alist))
       ,@body)))

(defconst eask-file-keywords
  '("package" "website-url" "keywords"
    "package-file" "files"
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
(defvar eask-file-root nil "The Eask file's directory.")

(defun eask-root-del (filename)
  "Remove Eask file root path from FILENAME."
  (when (stringp filename) (s-replace eask-file-root "" filename)))

(defun eask-file-load (location &optional noerror)
  "Load Eask file in the LOCATION."
  (when-let* ((target-eask-file (expand-file-name location user-emacs-directory))
              (result (eask--alias-env (load target-eask-file noerror t))))
    (setq eask-file target-eask-file  ; assign eask file only if success
          eask-file-root (file-name-directory target-eask-file))
    (run-hooks 'eask-file-loaded-hook)
    result))

(defun eask-file-try-load (relative-path)
  "Try load eask file in RELATIVE-PATH."
  (or (eask-file-load (concat relative-path "Easkfile") t)
      (eask-file-load (concat relative-path "Eask") t)))

(defun eask--print-env-info ()
  "Display environment information at the very top of the execution."
  (eask-msg "")
  (eask-msg "✓ Checking Emacs version %s... done!" emacs-version)
  (eask-with-verbosity 'debug
    (eask-msg "  ✓ Checking build number %s... done!" emacs-build-number)
    (eask-msg "  ✓ Checking system configuration %s... done!" system-configuration)
    (when-let ((emacs-build-time)
               (time (format-time-string "%Y-%m-%d" emacs-build-time)))
      (eask-msg "  ✓ Checking build time %s... done!" time)))
  (eask-msg "✓ Checking system %s... done!" system-type))

(defmacro eask--with-hooks (&rest body)
  "Execute BODY with before/after hooks."
  (declare (indent 0) (debug t))
  `(progn
     (run-hooks 'eask-before-command-hook)
     (run-hooks (intern (concat "eask-before-" (eask-command) "-hook")))
     ,@body
     (run-hooks (intern (concat "eask-after-" (eask-command) "-hook")))
     (run-hooks 'eask-after-command-hook)))

(defmacro eask-start (&rest body)
  "Execute BODY with workspace setup."
  (declare (indent 0) (debug t))
  `(unless eask-loading-file-p
     (if eask--initialized-p (progn ,@body)
       (setq eask--initialized-p t)
       (eask--setup-env
         (eask--handle-global-options)
         (eask--print-env-info)
         (cond
          ((eask-global-p)
           ;; We accept Eask file in global scope, but it shouldn't be used
           ;; as a sandbox.
           (if (eask-file-try-load "./")
               (eask-msg "✓ Loading config Eask file in %s... done!" eask-file)
             (eask-msg "✗ Loading config Eask file... missing!"))
           (message "")
           (package-activate-all)
           (eask-with-progress
             (ansi-green "Loading your configuration... ")
             (eask-with-verbosity 'debug
               (load (locate-user-emacs-file "early-init.el") t)
               (load (locate-user-emacs-file "../.emacs") t)
               (load (locate-user-emacs-file "init.el") t))
             (ansi-green "done"))
           (eask--with-hooks ,@body))
          (t
           (let* ((user-emacs-directory (expand-file-name (concat ".eask/" emacs-version "/")))
                  (package-user-dir (expand-file-name "elpa" user-emacs-directory))
                  (eask--first-init-p (not (file-directory-p user-emacs-directory)))
                  (user-init-file (locate-user-emacs-file "init.el"))
                  (custom-file (locate-user-emacs-file "custom.el"))
                  (special (eask-special-p)))
             (if (or (eask-file-try-load "../../")
                     special)
                 (progn
                   (if eask-file
                       (eask-msg "✓ Loading Eask file in %s... done!" eask-file)
                     (eask-msg "✗ Loading Eask file... missing!"))
                   (message "")
                   (package-activate-all)
                   (unless special
                     (ignore-errors (make-directory package-user-dir t))
                     (eask--silent (eask-setup-paths)))
                   (eask--with-hooks ,@body))
               (eask-msg "✗ Loading Eask file... missing!")
               (eask-help 'init)))))))))

;;
;;; Eask file

(defun eask-network-insecure-p ()
  "Are we attempt to use insecure connection?"
  (eq network-security-level 'low))

(defconst eask-source-mapping
  `((gnu          . "https://elpa.gnu.org/packages/")
    (nongnu       . "https://elpa.nongnu.org/nongnu/")
    (celpa        . "https://celpa.conao3.com/packages/")
    (jcs-elpa     . "https://jcs-emacs.github.io/jcs-elpa/packages/")
    (marmalade    . "https://marmalade-repo.org/packages/")
    (melpa        . "https://melpa.org/packages/")
    (melpa-stable . "https://stable.melpa.org/packages/")
    (org          . "https://orgmode.org/elpa/")
    (shmelpa      . "https://shmelpa.commandlinesystems.com/packages/"))
  "Mapping of source name and url.")

(defvar eask-package          nil)
(defvar eask-package-desc     nil)  ; package descriptor
(defvar eask-website-url      nil)
(defvar eask-keywords         nil)
(defvar eask-package-file     nil)
(defvar eask-files            nil)
(defvar eask-depends-on-emacs nil)
(defvar eask-depends-on       nil)
(defvar eask-depends-on-dev   nil)

(defun eask-package--get (key)
  "Return package info by KEY."
  (plist-get eask-package key))

(defun eask-package-name ()        (eask-package--get :name))
(defun eask-package-version ()     (eask-package--get :version))
(defun eask-package-description () (eask-package--get :description))

(defun eask-depends-emacs-version ()
  "Get Eask-file Emacs version string."
  (nth 0 (cdar eask-depends-on-emacs)))

(defun eask-package (name version description)
  "Set the package information."
  (if eask-package
      (eask-error "Multiple definition of `package'")
    (setq eask-package `(:name ,name :version ,version :description ,description))
    (progn  ; Run checker
      (eask--checker-string "Name" name)
      (version= version "0.1.0")
      (eask--checker-string "Description" description))))

(defun eask-website-url (url)
  "Set website URL."
  (if eask-website-url
      (eask-error "Multiple definition of `website-url'")
    (setq eask-website-url url)))

(defun eask-keywords (&rest keywords)
  "Set package keywords."
  (if eask-keywords
      (eask-error "Multiple definition of `keywords'")
    (setq eask-keywords keywords)))

(defun eask-package-file (file)
  "Set package file."
  (if eask-package-file
      (eask-error "Multiple definition of `package-file'")
    (setq eask-package-file (expand-file-name file))
    (let* ((package-file-exists (file-exists-p eask-package-file))
           (def-point (if (eask-pkg-el) "-pkg.el file" "package-file"))
           (target-file (cond ((eask-pkg-el) (expand-file-name (eask-pkg-el)))
                              (package-file-exists eask-package-file))))
      (unless package-file-exists
        (eask-warn "Package-file seems to be missing `%s'" file))
      (when target-file
        (with-temp-buffer
          (insert-file-contents target-file)
          (setq eask-package-desc (ignore-errors
                                    (if (eask-pkg-el)
                                        (package--read-pkg-desc 'dir)
                                      (package-buffer-info)))))
        (eask-msg (concat
                   (if eask-package-desc "✓ " "✗ ")
                   "Try constructing the package-descriptor (%s)... "
                   (if eask-package-desc "succeeded! " "failed!"))
                  (file-name-nondirectory target-file))))))

(defun eask-files (&rest patterns)
  "Set files patterns."
  (setq eask-files (append eask-files patterns)))

(defun eask-source (name &optional location)
  "Add archive NAME with LOCATION."
  (when (assoc name package-archives)
    (eask-error "Multiple definition of source `%s'" name))
  (setq location (or location (cdr (assq (intern name) eask-source-mapping))))
  (unless location (eask-error "Unknown package archive `%s'" name))
  (when (and location
             (gnutls-available-p)
             (not (eask-network-insecure-p)))
    (setq location (s-replace "https://" "http://" location)))
  (add-to-list 'package-archives (cons name location) t))

(defun eask-source-priority (archive-id &optional priority)
  "Add PRIORITY for to ARCHIVE-ID."
  (add-to-list 'package-archive-priorities (cons archive-id priority) t))

(defvar eask-depends-on-recipe-p nil
  "Set to t if package depends on recipe.")

(defun eask--setup-dependencies ()
  "Setup dependencies list."
  (setq eask-depends-on (reverse eask-depends-on)
        eask-depends-on-dev (reverse eask-depends-on-dev))
  (when eask-depends-on-recipe-p
    (eask-with-progress
      "✓ Checking local archives... "
      (eask-with-verbosity 'debug
        (add-to-list 'package-archives `("local" . ,github-elpa-archive-dir) t)
        ;; If the local archives is added, we set the priority to a very
        ;; high number so user we always use the specified dependencies!
        (add-to-list 'package-archive-priorities `("local" . 90) t))
      "done!")))

(add-hook 'eask-file-loaded-hook #'eask--setup-dependencies)

(defun eask-depends-on (pkg &rest args)
  "Specify a dependency of this package."
  (cond
   ((string= pkg "emacs")
    (if eask-depends-on-emacs
        (eask-error "Define dependencies with the same name `%s'" pkg)
      (let* ((minimum-version (car args))
             (recipe (list pkg minimum-version)))
        (if (version< emacs-version minimum-version)
            (eask-error "This requires Emacs %s and above!" minimum-version)
          (push recipe eask-depends-on-emacs))
        recipe)))
   ;; No argument specify
   ((<= (length args) 1)
    (let* ((minimum-version (or (car args) "latest"))
           (recipe (list pkg minimum-version)))
      (if (member recipe eask-depends-on)
          (eask-error "Define dependencies with the same name `%s'" pkg)
        (push recipe eask-depends-on))
      recipe))
   ;; recipe are entered
   (t
    (let ((recipe (append (list (intern pkg)) args)))
      (if (member recipe eask-depends-on)
          (eask-error "Define dependencies with the same name `%s'" pkg)
        (push recipe eask-depends-on)
        (eask-load "extern/github-elpa")
        (eask-with-verbosity 'debug
          (eask-with-progress
            (ansi-blue (format "Generating recipe for package %s... " (ansi-yellow pkg)))
            (write-region (pp-to-string recipe) nil (expand-file-name pkg github-elpa-recipes-dir))
            (ansi-blue "done ✓")))
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
  (dolist (dir dirs) (add-to-list 'load-path (expand-file-name dir) t)))

(defun eask-exec-paths (&rest dirs)
  "Add all DIRS to exec-path."
  (dolist (dir dirs) (add-to-list 'exec-path (expand-file-name dir) t)))

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
  "Trigger error event."
  (when (and (not eask--ignore-error-p)
             (not (eask-checker-p)))  ; ignore when checking Eask-file
    (if (eask-allow-error-p)  ; Trigger error at the right time
        (add-hook 'eask-after-command-hook #'eask--exit)
      (eask--exit))))

(defun eask--error (fnc &rest args)
  "On error."
  (let ((msg (eask--ansi 'error (apply #'format-message args))))
    (eask--unsilent (eask-msg "%s" msg))
    (run-hook-with-args 'eask-on-error-hook 'error msg)
    (eask--trigger-error))
  (when debug-on-error (apply fnc args)))

(advice-add 'error :around #'eask--error)

(defun eask--warn (fnc &rest args)
  "On warn."
  (let ((msg (eask--ansi 'warn (apply #'format-message args))))
    (eask--unsilent (eask-msg "%s" msg))
    (run-hook-with-args 'eask-on-warning-hook 'warn msg))
  (eask--silent (apply fnc args)))

(advice-add 'warn :around #'eask--warn)

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
(defun eask-error (msg &rest args) (apply #'eask--msg 'error "[ERROR]"   msg args))

(defun eask--ansi (symbol string)
  "Paint STRING with color defined by log level."
  (if-let ((ansi-function (cdr (assq symbol eask-level-color))))
      (funcall ansi-function string)
    string))

(defun eask--msg (symbol prefix msg &rest args)
  "If LEVEL is at or below `eask-verbosity', log message."
  (eask-with-verbosity symbol
    (let* ((string (apply #'eask--format prefix msg args))
           (output (eask--ansi symbol string))
           (func (cl-case symbol
                   ((or error warn) symbol)
                   (t #'message))))
      (funcall func "%s" output))))

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

(defun eask-report (&rest args)
  "Report error/warning depends on strict flag."
  (apply (if (eask-strict-p) #'eask-error #'eask-warn) args))

;;
;;; Log

(defconst eask-log-path ".log"
  "Directory path to create log files.")

(defcustom eask-log-file nil
  "Weather to generate log files."
  :type 'boolean)

(defmacro eask--log-write-buffer (buffer file)
  "Write BUFFER to FILE."
  `(when (get-buffer-create ,buffer)
     (let ((buffer-file-coding-system 'utf-8))
       (write-region (with-current-buffer ,buffer (buffer-string)) nil
                     (expand-file-name ,file log-dir)))))

(add-hook 'kill-emacs-hook  ; Write log files
          (lambda (&rest _)
            (when eask-log-file
              (let ((log-dir (expand-file-name eask-log-path eask-file-root)))
                (make-directory log-dir t)
                (eask--log-write-buffer "*Messages*" "messages.log")
                (eask--log-write-buffer "*Warnings*" "warnings.log")
                (eask--log-write-buffer "*Backtrace*" "backtrace.log")
                (eask--log-write-buffer "*Compile-Log*" "compile-log.log")))))

;;
;;; File

(defun eask-guess-package-name ()
  "Return the possible package name."
  (or (eask-package-name)
      (ignore-errors (file-name-nondirectory
                      (file-name-sans-extension eask-package-file)))))

(defun eask-files-spec ()
  "Return files spec."
  (or eask-files package-build-default-files-spec))

(defun eask-expand-file-specs (specs)
  "Expand file SPECS."
  (mapcar (lambda (elm) (expand-file-name (car elm) default-directory))
          (package-build-expand-file-specs default-directory specs nil t)))

(defun eask-package-files ()
  "Return package files in workspace."
  (let ((files (eask-expand-file-specs (eask-files-spec))))
    ;; Package file is part of package-files
    (when eask-package-file (push eask-package-file files))
    (delete-dups files)
    (setq files (cl-remove-if-not #'file-exists-p files))
    (unless files
      (eask-debug "No matching file(s) found in %s: %s" default-directory (eask-files-spec)))
    files))

(defun eask-package-el-files ()
  "Return package files in workspace."
  (cl-remove-if-not (lambda (filename) (string= (file-name-extension filename) "el")) (eask-package-files)))

(defun eask-package-elc-files ()
  "Return package files' elc in workspace."
  (when-let ((elcs (mapcar (lambda (elm) (concat elm "c")) (eask-package-el-files))))
    (setq elcs (cl-remove-if-not (lambda (elm) (file-exists-p elm)) elcs))
    elcs))

(defun eask-args-or-package-el-files ()
  "Return args if specified, else return package files by default."
  (if (eask-args)
      (eask-expand-file-specs (eask-args))
    (eask-package-el-files)))

(defun eask-package-multi-p ()
  "Return t if multi-files package."
  (< 1 (length (eask-package-files))))

(defun eask-package-single-p ()
  "Return t if single file package."
  (not (eask-package-multi-p)))

(defun eask-unpacked-size ()
  "Return unpacked size."
  (let ((size 0))
    (dolist (filename (eask-package-files))
      (cl-incf size (file-attribute-size (file-attributes filename))))
    (string-trim (ls-lisp-format-file-size size t))))

;;
;;; Progress

(defcustom eask-elapsed-time nil
  "Log with elapsed time."
  :type 'boolean)

(defcustom eask-minimum-reported-time 0.1
  "Minimal load time that will be reported."
  :type 'number)

(defmacro eask-with-progress (msg-start body msg-end)
  "Progress BODY wrapper with prefix and suffix messages."
  (declare (indent 0) (debug t))
  `(if eask-elapsed-time
       (let ((now (current-time)))
         (ignore-errors (eask-write ,msg-start)) ,body
         (let ((elapsed (float-time (time-subtract (current-time) now))))
           (if (< elapsed eask-minimum-reported-time)
               (ignore-errors (eask-msg ,msg-end))
             (ignore-errors (eask-write ,msg-end))
             (eask-msg (ansi-white (format " (%.3fs)" elapsed))))))
     (ignore-errors (eask-write ,msg-start)) ,body
     (ignore-errors (eask-msg ,msg-end))))

(defun eask-progress-seq (prefix sequence suffix func)
  "Progress SEQUENCE with messages."
  (let* ((total (length sequence)) (count 0)
         (offset (eask-2str (length (eask-2str total)))))
    (mapc
     (lambda (item)
       (cl-incf count)
       (eask-with-progress
         (format (concat "%s [%" offset "d/%d] %s... ") prefix count total
                 (ansi-green item))
         (when func (funcall func item))
         suffix))
     sequence)))

(defun eask-print-log-buffer (&optional buffer-or-name)
  "Loop through each line and print each line with corresponds log level."
  (with-current-buffer (or buffer-or-name (current-buffer))
    (goto-char (point-min))
    (while (not (eobp))
      (let ((line (buffer-substring-no-properties (line-beginning-position) (line-end-position))))
        (cond ((string-match-p "[: ][Ee]rror: " line) (eask-error line))
              ((string-match-p "[: ][Ww]arning: " line) (eask-warn line))
              (t (eask-log line))))
      (forward-line 1))))

;;
;;; Help

(defun eask-help (command)
  "Show help."
  (let* ((command (eask-2str command))  ; convert to string
         (help-file (concat eask-lisp-root "help/" command)))
    (if (file-exists-p help-file)
        (with-temp-buffer
          (insert-file-contents help-file)
          (unless (string= (buffer-string) "")
            (eask-msg (ansi-white (buffer-string)))))
      (eask-error "Help manual missig %s" help-file))))

(defun eask--print-no-matching-files ()
  "Print message for no matching files found."
  (eask-log "")
  (eask-log "Cannot find matching files with given pattern %s" (eask-args))
  (eask-log ""))

;;
;;; Checker

(defun eask--checker-existence ()
  "Return errors if required metadata is missing."
  (unless eask-package (eask-error "Missing metadata package; make sure you have create Eask-file with $ eask init!")))

(defun eask--check-strings (fmt f p &rest args)
  "Test strings (F and P); then print FMT if not equal."
  (unless (string= f p) (apply #'eask-warn (append (list fmt f p) args))))

(defun eask--check-optional (f p msg1 msg2 msg3 msg4)
  "Conditional way to check optional headers, URL and KEYWORDS ."
  (cond ((and f p) (eask--check-strings msg1 f p))
        (f (eask-warn msg2))
        (p (eask-warn msg3))
        (t (eask-warn msg4))))

(defun eask--checker-metadata ()
  "Report warnings if metadata doesn't match."
  (when-let* (((and eask-package eask-package-desc))
              (def-point (if (eask-pkg-el) "-pkg.el file" "package-file")))
    (eask--check-strings
     "Unmatched package name '%s'; it should be '%s'"
     (eask-package-name) (package-desc-name eask-package-desc))
    (when-let* ((ver-eask (eask-package-version))
                (ver-pkg (package-desc-version eask-package-desc))
                ;; `package-version-join' returns only one of the possible
                ;; inverses, since `version-to-list' is a many-to-one operation
                ((not (equal (version-to-list ver-eask) ver-pkg))))
      (eask--check-strings
       "Unmatched version '%s'; it should be '%s'"
       ver-eask (package-version-join ver-pkg)))
    (eask--check-strings
     "Unmatched summary '%s'; it should be '%s'"
     (eask-package-description) (package-desc-summary eask-package-desc))
    (let ((url (eask-package-desc-url)))
      (eask--check-optional
       eask-website-url url
       "Unmatched website URL '%s'; it should be '%s'"
       (format "Unmatched website URL '%s'; add ;; URL: %s to %s" eask-website-url eask-website-url def-point)
       (format "Unmatched website URL '%s'; add (website-url \"%s\") to Eask-file" url url)
       (format "URL header is optional, but it's often recommended")))
    (let ((keywords (eask-package-desc-keywords)))
      (cond
       ((or keywords eask-keywords)
        (dolist (keyword keywords)
          (unless (member keyword eask-keywords)
            (eask-warn "Unmatched keyword '%s'; add (keywords ... \"%s\") to Eask-file or consider removing it" keyword keyword)))
        (dolist (keyword eask-keywords)
          (unless (member keyword keywords)
            (eask-warn "Unmatched keyword '%s'; add ;; Keywords ... %s to %s or consider removing it" keyword keyword def-point))))
       (t
        (eask-warn "Keywords header is optional, but it's often recommended"))))
    (let* ((dependencies (append eask-depends-on-emacs eask-depends-on))
           (dependencies (mapcar #'car dependencies))
           (dependencies (mapcar (lambda (elm) (eask-2str elm)) dependencies))
           (requirements (package-desc-reqs eask-package-desc))
           (requirements (mapcar #'car requirements))
           (requirements (mapcar (lambda (elm) (eask-2str elm)) requirements)))
      (dolist (req requirements)
        (unless (member req dependencies)
          (eask-warn "Unmatched dependency '%s'; add (depends-on \"%s\") to Eask-file or consider removing it" req req)))
      (dolist (dep dependencies)
        (unless (member dep requirements)
          (eask-warn "Unmatched dependency '%s'; add (%s \"VERSION\") to %s or consider removing it" dep dep def-point))))))

(add-hook 'eask-file-loaded-hook #'eask--checker-existence)
(add-hook 'eask-file-loaded-hook #'eask--checker-metadata)

(defun eask--checker-string (name var)
  "Run checker for VAR."
  (unless (stringp var)
    (eask-error "%s must be a string" name))
  (when (string-empty-p var)
    (eask-warn "%s cannot be an empty string" name)))

;;
;;; User customization

(defcustom eask-file-loaded-hook nil
  "Hook runs after Easkfile is loaded."
  :type 'hook)

(defcustom eask-before-command-hook nil
  "Hook runs before command is executed."
  :type 'hook)

(defcustom eask-after-command-hook nil
  "Hook runs after command is executed."
  :type 'hook)

(defcustom eask-on-error-hook nil
  "Hook runs when error is triggered."
  :type 'hook)

(defcustom eask-on-warning-hook nil
  "Hook runs when warning is triggered."
  :type 'hook)

(defcustom eask-dist-path "dist"
  "Name of default target directory for building packages."
  :type 'string)

;;
;;; Externals

(eask-load "extern/ansi")
(with-eval-after-load 'ansi (eask-load "extern/ansi"))  ; override
(eask-load "extern/package")
(eask-load "extern/package-build")
(eask-load "extern/s")

;;
;;; Requirement

(when (version< emacs-version "26.1")
  (eask-error "Eask requires Emacs 26.1 and above!"))

;;; _prepare.el ends here

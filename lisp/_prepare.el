;;; _prepare.el --- Prepare for command tasks  -*- lexical-binding: t; -*-
;;; Commentary: Prepare to setup Eask environment for sandboxing
;;; Code:

(require 'ansi-color)
(require 'package)
(require 'project)
(require 'json)
(require 'nsm)
(require 'url-vars)

(require 'cl-lib)
(require 'files)
(require 'ls-lisp)
(require 'pp)
(require 'rect)
(require 'subr-x)

;; Determine the underlying operating system
(defconst eask-is-windows (memq system-type '(cygwin windows-nt ms-dos))
  "The system is Windows.")
(defconst eask-is-mac     (eq system-type 'darwin)
  "The system is macOS.")
(defconst eask-is-linux   (eq system-type 'gnu/linux)
  "The system is GNU Linux.")
(defconst eask-is-bsd     (or eask-is-mac (eq system-type 'berkeley-unix))
  "The system is BSD.")

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
  "Prevent `_prepare.el' loading twice.

Arguments FNC and ARGS are used for advice `:around'."
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
  (let* ((script (ignore-errors (file-name-directory eask--script)))
         (dir (ignore-errors (expand-file-name (concat script "../"))))
         (basename (file-name-nondirectory (directory-file-name dir)))
         (root (expand-file-name "/")))
    (while (and (not (string= root dir))
                (not (string= basename "lisp")))
      (setq dir (expand-file-name (concat dir "../"))
            basename (file-name-nondirectory (directory-file-name dir))))
    dir)
  "Source `lisp' directory; should always end with slash.")

(defun eask-command ()
  "What's the current command?

If the command is with subcommand, it will return command with concatenate with
slash separator.  For example, the following:

   $ eask lint checkdoc [FILES..]

will return `lint/checkdoc' with a dash between two subcommands."
  (let* ((script-dir (file-name-directory eask--script))
         (script-file (file-name-sans-extension (file-name-nondirectory eask--script)))
         (module-name (eask-s-replace eask-lisp-root "" script-dir))
         (module-names (split-string module-name "/" t)))
    ;; Make certain commands the root command; e.g. `core', `checker', etc.
    (if (member (nth 0 module-names) '("core" "checker")) script-file
      (mapconcat #'identity (append module-names
                                    (list script-file))
                 "/"))))

(defun eask-special-p ()
  "Return t if the command that can be run without Eask-file existence."
  (member (eask-command) '("init/cask" "init/keg"
                           "init/source"
                           "cat" "keywords"
                           "generate/ignore" "generate/license")))

(defun eask-checker-p ()
  "Return t if running Eask as the checker."
  (member (eask-command) '("check-eask")))

(defun eask-script (script)
  "Return full SCRIPT filename."
  (concat eask-lisp-root script ".el"))

(defvar eask-loading-file-p nil
  "This became t; if we are loading script from another file and not expecting
the `eask-start' execution.")

(defun eask-load (script)
  "Load another eask SCRIPT; so we can reuse functions across all scripts."
  (let ((eask-loading-file-p t)) (eask-call script)))

(defun eask-call (script)
  "Call another eask SCRIPT."
  (if-let* ((script-file (eask-script script))
            ((file-exists-p script-file)))
      (load script-file nil t)
    (eask-error "Script missing %s" script-file)))

;;
;;; Util

(defmacro eask-defvc< (version &rest body)
  "Define scope if Emacs version is below VERSION.

Argument BODY are forms for execution."
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
    (if (<= len 1) form-1 form-2)))

;; This is used to creating the directory recipe!
(defun eask-current-time ()
  "Return current time."
  (let ((now (current-time))) (logior (ash (car now) 16) (cadr now))))

(defun eask-seq-str-max (sequence)
  "Return max length in SEQUENCE of strings."
  (let ((result 0))
    (mapc (lambda (elm) (setq result (max result (length (eask-2str elm))))) sequence)
    result))

(defun eask-f-filename (path)
  "Return the name of PATH."
  (file-name-nondirectory (directory-file-name path)))

(defun eask-s-replace (old new s)
  "Replace OLD with NEW in S each time it occurs."
  (if (fboundp #'string-replace)
      (string-replace old new s)
    (replace-regexp-in-string (regexp-quote old) new s t t)))

;;
;;; Archive

(defun eask--download-archives ()
  "If archives download failed; download it manually."
  (dolist (archive package-archives)
    (let* ((name (car archive))
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
  "Add all bin directory to the variable `exec-path'."
  (dolist (entry (directory-files package-user-dir t directory-files-no-dot-files-regexp))
    (when-let* ((bin (expand-file-name "bin" entry))
                ((file-directory-p bin)))
      (add-to-list 'exec-path bin t)))
  (delete-dups exec-path))

(defun eask--update-load-path ()
  "Add all .el files to the variable `load-path'."
  (dolist (filename (eask-package-el-files))
    (add-to-list 'load-path (file-name-directory filename) t))
  (delete-dups load-path))

(defun eask-dependencies ()
  "Return list of dependencies."
  (append eask-depends-on (and (eask-dev-p) eask-depends-on-dev)))

(defun eask--package-mapc (func deps)
  "Like function `mapc' but for process package transaction specifically.

For arguments FUNC and DEPS, see function `mapc' for more information."
  (let* ((eask--package-prefix)  ; remain untouch
         (len (length deps))
         (len-str (eask-2str len))
         (fmt (concat "[%" (eask-2str (length len-str)) "d/" len-str "] "))
         (count 0))
    (dolist (pkg deps)
      (cl-incf count)
      (setq eask--package-prefix (format fmt count))
      (funcall func pkg))))

(defun eask--install-deps (dependencies msg)
  "Install DEPENDENCIES.

Argument MSG are extra information to display in the header; mainly define the
scope of the dependencies (it's either `production' or `development')."
  (let* ((names (mapcar #'car dependencies))
         (names (mapcar #'eask-intern names))
         (len (length dependencies))
         (ies (eask--sinr len "y" "ies"))
         (pkg-installed (cl-remove-if #'package-installed-p names))
         (installed (length pkg-installed)) (skipped (- len installed)))
    (eask-log "Installing %s %s dependenc%s..." len msg ies)
    (eask-msg "")
    (eask--package-mapc #'eask-package-install names)
    (eask-msg "")
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
    (eask-msg "")
    (eask--install-deps eask-depends-on-dev "development")))

(defun eask-setup-paths ()
  "Setup both variables `exec-path' and `load-path'."
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
  "Package initialization.

If the argument FORCE is non-nil, force initialize packages in this session."
  (when (or (not package--initialized) (not package-archive-contents) force
            ;; XXX we need to initialize once in global scope since most Emacs
            ;; configuration would likely to set `package-archives' variable
            ;; themselves.
            (and (eask-config-p) (not eask--package-initialized)))
    (setq eask--package-initialized t)
    (eask-with-progress
      (ansi-green "Loading package information... ")
      (eask-with-verbosity 'debug
        (package-initialize t) (package-refresh-contents)
        (eask--download-archives))
      (ansi-green "done ✓"))))

(defun eask--pkg-transaction-vars (pkg)
  "Return 1 symbol and 2 strings.

Argument PKG is the name of the package."
  (let* (;; Ensure symbol
         (pkg (eask-intern pkg))
         ;; Wrap package name with color
         (pkg-string (ansi-green (eask-2str pkg)))
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
  "Scope that temporary make ARCHIVES available.

Argument BODY are forms for execution."
  (declare (indent 1) (debug t))
  `(let ((package-archives package-archives)
         (archives (eask-listify ,archives))
         (added))
     (dolist (archive archives)
       (unless (assoc archive package-archives)
         (setq added t)
         (eask-with-progress
           (format "Adding required archives (%s)... " (ansi-yellow archive))
           (eask-f-source archive)
           "done ✓")))
     (when added
       (eask-with-progress
         "Refresh archives information... "
         (eask--silent (eask-pkg-init t))
         "done ✓"))
     ,@body))

(defun eask-package-installable-p (pkg)
  "Return non-nil if package (PKG) is installable."
  (assq (eask-intern pkg) package-archive-contents))

(defvar eask--package-prefix ""
  "The prefix to display before each package action.")

(defun eask-package-install (pkg)
  "Install the package (PKG)."
  (eask-defvc< 27 (eask-pkg-init))  ; XXX: remove this after we drop 26.x
  (eask--pkg-process pkg
    (cond
     ((package-installed-p pkg)
      (eask-msg "  - %sSkipping %s (%s)... already installed ✗"
                eask--package-prefix
                name version))
     ((progn
        (eask-pkg-init)
        (unless (eask-package-installable-p pkg)
          (eask-error "Package not installable `%s'; make sure package archive is included" pkg))))
     ((when-let* ((desc (eask-package-desc pkg))
                  (req-emacs (assoc 'emacs (package-desc-reqs desc)))
                  (req-emacs (package-version-join (nth 0 (cdr req-emacs))))
                  ((version< emacs-version req-emacs)))
        (if (eask-strict-p)
            (eask-error "  - %sSkipping %s (%s)... it requires Emacs %s and above ✗"
                        eask--package-prefix
                        pkg (eask-package--version-string pkg) emacs-version)
          (eask-msg "  - %sSkipping %s (%s)... it requires Emacs %s and above ✗"
                    eask--package-prefix
                    name version (ansi-yellow emacs-version)))))
     (t
      (eask--pkg-process pkg
        (eask-with-progress
          (format "  - %sInstalling %s (%s)... " eask--package-prefix name version)
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
  "Delete the package (PKG)."
  (eask-defvc< 27 (eask-pkg-init))  ; XXX: remove this after we drop 26.x
  (eask--pkg-process pkg
    (cond
     ((not (package-installed-p pkg))
      (eask-msg "  - %sSkipping %s (%s)... not installed ✗" eask--package-prefix name version))
     (t
      (eask--pkg-process pkg
        (eask-with-progress
          (format "  - %sUninstalling %s (%s)... " eask--package-prefix name version)
          (eask-with-verbosity 'debug
            (package-delete (eask-package-desc pkg t) (eask-force-p)))
          "done ✓"))))))

(defun eask-package-reinstall (pkg)
  "Reinstall the package (PKG)."
  (eask-defvc< 27 (eask-pkg-init))  ; XXX: remove this after we drop 26.x
  (eask--pkg-process pkg
    (cond
     ((not (package-installed-p pkg))
      (eask-msg "  - %sSkipping %s (%s)... not installed ✗" eask--package-prefix name version))
     (t
      (eask-pkg-init)
      (eask--pkg-process pkg
        (eask-with-progress
          (format "  - %sReinstalling %s (%s)... " eask--package-prefix name version)
          (eask-with-verbosity 'debug
            (package-delete (eask-package-desc pkg t) t)
            (eask-ignore-errors (package-install pkg)))
          "done ✓"))))))

(defun eask-package-desc (name &optional current)
  "Build package description by its NAME.

The argument NAME must be the package's name.  If the argument CURRENT is
non-nil, we retrieve version number from the variable `package-alist';
otherwise, we retrieve it from the variable `package-archive-contents'."
  (cadr (assq name (if current package-alist
                     (or package-archive-contents package-alist)))))

(defun eask-package--version (name &optional current)
  "Return package's version.

For arguments NAME and CURRENT, please see function `eask-package-desc' for
full detials."
  (when-let ((desc (eask-package-desc name current)))
    (package-desc-version desc)))

(defun eask-package--version-string (pkg)
  "Return PKG's version."
  (if-let ((version (or (eask-package--version pkg t)
                        (eask-package--version pkg nil))))
      (package-version-join version)
    ;; Just in case, but this should never happens!
    "0"))

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

(defconst eask-homedir (getenv "EASK_HOMEDIR")
  "Eask temporary storage.")

(defconst eask-invocation (getenv "EASK_INVOCATION")
  "Eask invocation program.")

;;
;;; Flags

(defun eask--str2num (str)
  "Convert string (STR) to number."
  (ignore-errors (string-to-number str)))

(defun eask--flag (flag)
  "Return non-nil if FLAG exists.."
  (member (concat "--eask" flag) eask-argv))

(defun eask--flag-value (flag)
  "Return value for FLAG."
  (nth 1 (eask--flag flag)))

;;; Boolean
(defun eask-global-p ()
  "Non-nil when in global space (`-g', `--global')."
  (eask--flag "-g"))
(defun eask-config-p ()
  "Non-nil when in config space (`-c', `--config')."
  (eask--flag "-c"))
(defun eask-local-p ()
  "Non-nil when in local space (default)."
  (and (not (eask-global-p))
       (not (eask-config-p))))
(defun eask-all-p ()
  "Non-nil when flag is on (`-a', `--all')."
  (eask--flag "-a"))
(defun eask-quick-p ()
  "Non-nil when flag is on (`-q', `--quick')."
  (eask--flag "-q"))
(defun eask-force-p ()
  "Non-nil when flag is on (`-f', `--force')."
  (eask--flag "-f"))
(defun eask-dev-p ()
  "Non-nil when flag is on (`--dev', `--development')."
  (eask--flag "--dev"))
(defun eask-debug-p ()
  "Non-nil when flag is on (`--debug')."
  (eask--flag "--debug"))
(defun eask-strict-p ()
  "Non-nil when flag is on (`--strict')."
  (eask--flag "--strict"))
(defun eask-timestamps-p ()
  "Non-nil when flag is on (`--timestamps')."
  (eask--flag "--timestamps"))
(defun eask-log-level-p ()
  "Non-nil when flag is on (`--log-level')."
  (eask--flag "--log-level"))
(defun eask-log-file-p ()
  "Non-nil when flag is on (`--log-file', `--lf')."
  (eask--flag "--log-file"))
(defun eask-elapsed-time-p ()
  "Non-nil when flag is on (`--elapsed-time', `--et')."
  (eask--flag "--elapsed-time"))
(defun eask-allow-error-p ()
  "Non-nil when flag is on (`--allow-error')."
  (eask--flag "--allow-error"))
(defun eask-insecure-p ()
  "Non-nil when flag is on (`--insecure')."
  (eask--flag "--insecure"))
(defun eask-no-color-p ()
  "Non-nil when flag is on (`--no-color')."
  (eask--flag "--no-color"))
(defun eask-clean-p ()
  "Non-nil when flag is on (`-c', `--clean')."
  (eask--flag "--clean"))
(defun eask-json-p ()
  "Non-nil when flag is on (`--json')."
  (eask--flag "--json"))
(defun eask-number-p ()
  "Non-nil when flag is on (`-n', `--number')."
  (eask--flag "--number"))

;;; String (with arguments)
(defun eask-output ()
  "Non-nil when flag has value (`--o', `--output')."
  (eask--flag-value "--output"))
(defun eask-proxy ()
  "Non-nil when flag has value (`--proxy')."
  (eask--flag-value "--proxy"))        ; --proxy
(defun eask-http-proxy ()
  "Non-nil when flag has value (`--http-proxy')."
  (eask--flag-value "--http-proxy"))
(defun eask-https-proxy ()
  "Non-nil when flag has value (`--https-proxy')."
  (eask--flag-value "--https-proxy"))
(defun eask-no-proxy ()
  "Non-nil when flag has value (`--no-proxy')."
  (eask--flag-value "--no-proxy"))
(defun eask-destination ()
  "Non-nil when flag has value (`--dest', `--destination')."
  (eask--flag-value "--dest"))
(defalias 'eask-dest #'eask-destination
  "Shorthand for function `eask-destination'.")
(defun eask-from ()
  "Non-nil when flag has value (`--from')."
  (eask--flag-value "--from"))

;;; Number (with arguments)
(defun eask-depth ()
  "Non-nil when flag has value (`--depth')."
  (eask--str2num (eask--flag-value "--depth")))
(defun eask-verbose ()
  "Non-nil when flag has value (`-v', `--verbose')."
  (eask--str2num (eask--flag-value "--verbose")))

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
  "Add proxy.

Argument PROTOCAL and HOST are used to construct scheme."
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
   '("-g" "-c" "-a" "-q" "-f" "--dev"
     "--debug" "--strict"
     "--allow-error"
     "--insecure"
     "--timestamps" "--log-level"
     "--log-file"
     "--elapsed-time"
     "--no-color"
     "--clean"
     "--json"
     "--number"))
  "List of boolean type options.")

(defconst eask--option-args
  (eask--form-options
   '("--output"
     "--proxy" "--http-proxy" "--https-proxy" "--no-proxy"
     "--verbose" "--silent"
     "--depth" "--dest" "--from"))
  "List of arguments (number/string) type options.")

(defconst eask--command-list
  (append eask--option-switches eask--option-args)
  "List of commands to accept, so we can avoid unknown option error.")

(defun eask-self-command-p (arg)
  "Return non-nil if ARG is known internal command."
  (member arg eask--command-list))

(defun eask-argv (index)
  "Return argument value by INDEX."
  (elt eask-argv index))

(defun eask-argv-out ()
  "Convert all internal arguments to external arguments.

Simply remove `--eask' for each option, like `--eask--strict' to `--strict'."
  (mapcar (lambda (arg)
            (eask-s-replace "--eask" "" arg))
          eask-argv))

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
  "Execute forms BODY in batch-mode."
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
           (default-directory (cond ((eask-global-p) eask-homedir)
                                    ((eask-config-p) user-emacs-directory)
                                    (t default-directory)))
           (alist))
       (dolist (cmd eask--command-list)
         (push (cons cmd (lambda (&rest _))) alist))
       (setq command-switch-alist (append command-switch-alist alist))
       ,@body)))

(defconst eask-file-keywords
  '("package" "website-url" "keywords"
    "author" "license"
    "package-file" "package-descriptor" "files"
    "script"
    "source" "source-priority"
    "depends-on" "development"
    "exec-paths" "load-paths")
  "List of Eask file's DSL keywords.")

(defun eask--loop-file-keywords (func)
  "Loop through Eask file keywords for environment replacement.

Argument FUNC is a function we execute while this function will provide the new
and old function name.

Internal used for function `eask--alias-env'."
  (dolist (keyword eask-file-keywords)
    (let ((keyword-sym (intern keyword))
          (api (intern (concat "eask-f-" keyword)))    ; existing function
          (old (intern (concat "eask--f-" keyword))))  ; variable that holds function pointer
      (funcall func keyword-sym api old))))

(defmacro eask--alias-env (&rest body)
  "Replace all Eask file functions temporary; this is only used when loading
Eask file in the workspace.

Argument BODY are forms for execution."
  (declare (indent 0) (debug t))
  `(let (result)
     ;; XXX: Magic here is we replace all keyword functions with `eask-xxx'...
     (eask--loop-file-keywords
      (lambda (keyword api old)
        (defalias old (symbol-function keyword))
        (defalias keyword (symbol-function api))))
     (setq result (progn ,@body))
     ;; XXX: after loading Eask file, we revert those functions back to normal!
     (eask--loop-file-keywords
      (lambda (keyword _api old)
        (defalias keyword (symbol-function old))))
     result))

(defvar eask-file nil "The Eask file's filename.")
(defvar eask-file-root nil "The Eask file's directory.")

(defun eask-root-del (filename)
  "Remove Eask file root path from FILENAME."
  (when (stringp filename)
    (eask-s-replace (or eask-file-root default-directory) "" filename)))

(defun eask-file-load (location &optional noerror)
  "Load Eask file in the LOCATION.

Argument NOERROR is passed through function `load'; therefore, please see the
function `load' for more detials."
  (when-let* ((target-eask-file (expand-file-name location user-emacs-directory))
              (result (eask--alias-env (load target-eask-file noerror t t))))
    (setq eask-file target-eask-file  ; assign eask file only if success
          eask-file-root (file-name-directory target-eask-file))
    (run-hooks 'eask-file-loaded-hook)
    result))

(defun eask--match-file (name)
  "Check to see if NAME is our target Eask-file, then return it."
  (let (;; Ensure path to filename
        (name             (file-name-nondirectory (directory-file-name name)))
        ;; `p-' stards for pattern
        (p-easkfile-full  (format "Easkfile.%s" emacs-version))
        (p-easkfile-major (format "Easkfile.%s" emacs-major-version))
        (p-easkfile       "Easkfile")
        (p-eask-full      (format "Eask.%s" emacs-version))
        (p-eask-major     (format "Eask.%s" emacs-major-version))
        (p-eask           "Eask"))
    (car (member name (list p-easkfile-full p-easkfile-major p-easkfile
                            p-eask-full p-eask-major p-eask)))))

(defun eask--all-files (&optional dir)
  "Return a list of Eask files from DIR.

If argument DIR is nil, we use `default-directory' instead."
  (setq dir (or dir default-directory))
  (when-let* ((files (append
                      (ignore-errors (directory-files dir t "Easkfile[.0-9]*\\'"))
                      (ignore-errors (directory-files dir t "Eask[.0-9]*\\'"))))
              (files (cl-remove-if #'file-directory-p files)))
    (cl-remove-if-not #'eask--match-file files)))

(defun eask--find-files (start-path)
  "Find the Eask-file from START-PATH.

This uses function `locate-dominating-file' to look up directory tree."
  (when-let*
      (;; XXX: This is redundant, but the simplest way to find the root path!
       (root (locate-dominating-file start-path #'eask--all-files))
       (files (eask--all-files root))  ; get all available Eask-files
       ;; Filter it to restrict to this Emacs version!
       (files (cl-remove-if-not #'eask--match-file files))
       ;; Make `Easkfile.29.1' > `Easkfile.29' > `Easkfile' (same with `Eask' file)
       (files (sort files #'string-greaterp))
       ;; Make `Easkfile' > `Eask' higher precedent!
       (files (sort files (lambda (item1 item2)
                            (and (string-prefix-p "Easkfile" item1)
                                 (not (string-prefix-p "Easkfile" item2)))))))
    files))

(defun eask-file-try-load (start-path)
  "Try load the Eask-file in START-PATH."
  (when-let* ((files (eask--find-files start-path))
              (file (car files)))
    (eask-file-load file)))

(defmacro eask--with-hooks (&rest body)
  "Execute BODY with before/after hooks."
  (declare (indent 0) (debug t))
  `(let* ((command (eask-command))
          (before  (concat "eask-before-" command "-hook"))
          (after   (concat "eask-after-" command "-hook")))
     (run-hooks 'eask-before-command-hook)
     (run-hooks (intern before))
     ,@body
     (run-hooks (intern after))
     (run-hooks 'eask-after-command-hook)))

(defmacro eask--setup-home (dir &rest body)
  "Set up config directory in DIR, then execute BODY."
  (declare (indent 1) (debug t))
  `(let* ((user-emacs-directory (expand-file-name (concat ".eask/" emacs-version "/") ,dir))
          (package-user-dir (expand-file-name "elpa" user-emacs-directory))
          (early-init-file (locate-user-emacs-file "early-init.el"))
          (user-init-file (locate-user-emacs-file "init.el"))
          (custom-file (locate-user-emacs-file "custom.el")))
     ,@body))

(defmacro eask-start (&rest body)
  "Execute BODY with workspace setup."
  (declare (indent 0) (debug t))
  `(unless eask-loading-file-p
     (if eask--initialized-p (progn ,@body)
       (setq eask--initialized-p t)
       (eask--setup-env
         (eask--handle-global-options)
         (cond
          ((or (eask-global-p) (eask-special-p))  ; Commands without Eask-file needed
           (eask--setup-home (concat eask-homedir "../")  ; `/home/user/', escape `.eask'
             (let ((eask--first-init-p (not (file-directory-p user-emacs-directory))))
               ;; We accept Eask-file in `global' scope, but it shouldn't be used
               ;; for the sandbox.
               (eask-with-verbosity 'debug
                 (if (eask-file-try-load "./")
                     (eask-msg "✓ Loading global Eask file in %s... done!" eask-file)
                   (eask-msg "✗ Loading global Eask file... missing!"))
                 (message ""))
               (package-activate-all)
               (ignore-errors (make-directory package-user-dir t))
               (eask--with-hooks ,@body))))
          ((eask-config-p)
           (let ((inhibit-config (eask-quick-p)))
             ;; We accept Eask-file in `config' scope, but it shouldn't be used
             ;; for the sandbox.
             (eask-with-verbosity 'debug
               (if (eask-file-try-load "./")
                   (eask-msg "✓ Loading config Eask file in %s... done!" eask-file)
                 (eask-msg "✗ Loading config Eask file... missing!"))
               (message ""))
             (package-activate-all)
             (eask-with-progress
               (ansi-green "Loading your configuration... ")
               (eask-with-verbosity 'all
                 (unless inhibit-config
                   (load (locate-user-emacs-file "early-init.el") t)
                   (load (locate-user-emacs-file "../.emacs") t)
                   (load (locate-user-emacs-file "init.el") t)))
               (ansi-green (if inhibit-config "skipped ✗" "done ✓")))
             (eask--with-hooks ,@body)))
          (t
           (eask--setup-home nil  ; `nil' is the `default-directory'
             (let ((eask--first-init-p (not (file-directory-p user-emacs-directory))))
               (eask-with-verbosity 'debug
                 (if (eask-file-try-load "./")
                     (eask-msg "✓ Loading Eask file in %s... done!" eask-file)
                   (eask-msg "✗ Loading Eask file... missing!"))
                 (message ""))
               (if (not eask-file)
                   (eask-help "core/init")
                 (package-activate-all)
                 (ignore-errors (make-directory package-user-dir t))
                 (eask--silent (eask-setup-paths))
                 (eask--with-hooks ,@body))))))))))

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

(defvar eask-package            nil)
(defvar eask-package-desc       nil)  ; package descriptor
(defvar eask-package-descriptor nil)
(defvar eask-website-url        nil)
(defvar eask-keywords           nil)
(defvar eask-authors            nil)
(defvar eask-licenses           nil)
(defvar eask-package-file       nil)
(defvar eask-files              nil)
(defvar eask-scripts            nil)
(defvar eask-depends-on-emacs   nil)
(defvar eask-depends-on         nil)
(defvar eask-depends-on-dev     nil)

(defmacro eask--save-eask-file-state (&rest body)
  "Execute BODY without touching the Eask-file global variables."
  (declare (indent 0) (debug t))
  `(let (package-archives
         package-archive-priorities
         eask-file
         eask-file-root
         eask-package
         eask-package-desc
         eask-website-url
         eask-keywords
         eask-authors
         eask-licenses
         eask-package-file
         eask-package-descriptor
         eask-files
         eask-scripts
         eask-depends-on-emacs
         eask-depends-on
         eask-depends-on-dev)
     ,@body))

(defmacro eask--save-load-eask-file (file success &rest error)
  "Load an Eask FILE and execute forms SUCCESS or ERROR."
  (declare (indent 2) (debug t))
  `(eask--save-eask-file-state
     (eask--setup-env
       (eask--alias-env
         (if (let ((default-directory (file-name-directory ,file)))
               (ignore-errors (eask-file-load ,file)))
             (progn ,success)
           ,@error)))))

(defun eask-package--get (key)
  "Return package info by KEY."
  (plist-get eask-package key))

(defun eask-package-name ()
  "Return current package's name."
  (eask-package--get :name))
(defun eask-package-version ()
  "Return current package's version number."
  (eask-package--get :version))
(defun eask-package-description ()
  "Return current package's description."
  (eask-package--get :description))

(defun eask-depends-emacs-version ()
  "Get Eask-file Emacs version string."
  (nth 0 (cdar eask-depends-on-emacs)))

(defun eask-f-package (name version description)
  "Set the package information.

Argument NAME is the name of the package.  VERSION is the string contains valid
version number.  DESCRIPTION is the package description."
  (if eask-package
      (eask-error "Multiple definition of `package'")
    (setq eask-package `(:name ,name :version ,version :description ,description))
    (progn  ; Run checker
      (eask--checker-string "Name" name)
      (version= version "0.1.0")
      (eask--checker-string "Description" description))))

(defun eask-f-website-url (url)
  "Set website URL."
  (if eask-website-url
      (eask-error "Multiple definition of `website-url'")
    (setq eask-website-url url)))

(defun eask-f-keywords (&rest keywords)
  "Set package KEYWORDS."
  (if eask-keywords
      (eask-error "Multiple definition of `keywords'")
    (setq eask-keywords keywords)))

(defun eask-f-author (name &optional email)
  "Set package author's NAME and EMAIL."
  (if (member name (mapcar #'car eask-authors))
      (eask-warn "Warning regarding duplicate author name, %s" name)
    (when (and email
               (not (string-match-p "@" email)))
      (eask-warn "Email seems to be invalid, %s" email))
    (push (cons name email) eask-authors)))

(defun eask-f-license (name)
  "Set package license NAME."
  (if (member name eask-licenses)
      (eask-warn "Warning regarding duplicate license name, %s" name)
    (push name eask-licenses)))

(defun eask--try-construct-package-desc (file)
  "Try construct the package descriptor from FILE."
  (let (skipped)
    (with-temp-buffer
      (insert-file-contents file)
      (setq eask-package-desc
            (ignore-errors
              (cond ((string-suffix-p "-pkg.el" file)  ; if ensure -pkg.el
                     (package--read-pkg-desc 'dir))
                    ((eask-pkg-el)                     ; if -pkg.el is presented,
                     (setq skipped t) nil)             ; skip it
                    (t (package-buffer-info))))))      ; default read main package file
    (eask-msg (concat
               (if eask-package-desc "✓ " "✗ ")
               "Try constructing the package-descriptor (%s)... "
               (cond (eask-package-desc "succeeded!")
                     (skipped "skipped!")
                     (t "failed!")))
              (file-name-nondirectory file))))

(defun eask-f-package-file (file)
  "Set package FILE."
  (if eask-package-file
      (eask-error "Multiple definition of `package-file'")
    (setq eask-package-file (expand-file-name file))
    (if (file-exists-p eask-package-file)
        (eask--try-construct-package-desc eask-package-file)
      (eask-warn "Package-file seems to be missing `%s'" file))
    (when-let
        (((and (not eask-package-descriptor)  ; prevent multiple definition error
               (not eask-package-desc)))      ; check if constructed
         (pkg-file (eask-pkg-el)))
      (eask-f-package-descriptor pkg-file)
      ;; XXX: Make sure DSL package descriptor is set back to `nil'
      (setq eask-package-descriptor nil))))

(defun eask-f-package-descriptor (pkg-file)
  "Set package PKG-FILE."
  (cond
   (eask-package-descriptor
    (eask-error "Multiple definition of `package-descriptor'"))
   ((and eask-package-desc                ; check if construct successfully
         (equal (eask-pkg-el) pkg-file))  ; check filename the same
    )                                     ; ignore
   (t
    (setq eask-package-descriptor (expand-file-name pkg-file))
    (cond ((not (string-suffix-p "-pkg.el" eask-package-descriptor))
           (eask-error "Pkg-file must end with `-pkg.el'"))
          ((not (file-exists-p eask-package-descriptor))
           (eask-warn "Pkg-file seems to be missing `%s'" pkg-file))
          (t
           (eask--try-construct-package-desc eask-package-descriptor))))))

(defun eask-f-files (&rest patterns)
  "Set files PATTERNS."
  (setq eask-files (append eask-files patterns)))

(defun eask-f-script (name command &rest args)
  "Add a script command.

Argument NAME is the command id, and cannot be repeated.  Argument COMMAND is
a string contain shell commands.  The rest arguments ARGS is a list of string
contains extra shell commands, and it will eventually be concatenate with the
argument COMMAND."
  (when (symbolp name) (setq name (eask-2str name)))  ; ensure to string, accept symbol
  (when (assoc name eask-scripts)
    (eask-error "Run-script with the same key name is not allowed: `%s`" name))
  (push (cons name
              (mapconcat #'identity (append (list command) args) " "))
        eask-scripts))

(defun eask-f-source (name &optional location)
  "Add archive NAME with LOCATION."
  (when (symbolp name) (setq name (eask-2str name)))  ; ensure to string, accept symbol
  (when (assoc name package-archives)
    (eask-error "Multiple definition of source `%s'" name))
  (setq location (or location (cdr (assq (intern name) eask-source-mapping))))
  (unless location (eask-error "Unknown package archive `%s'" name))
  (when (and location
             (gnutls-available-p)
             (not (eask-network-insecure-p)))
    (setq location (eask-s-replace "https://" "http://" location)))
  (add-to-list 'package-archives (cons name location) t))

(defun eask-f-source-priority (archive-id &optional priority)
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

(defun eask-f-depends-on (pkg &rest args)
  "Specify a dependency (PKG) of this package.

Argument PKG is the name of that dependency.  ARGS can either be a string
contains the version number or a list contains recipe information (for local
ELPA)."
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
    (let* ((minimum-version (or (car args) "0"))
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

(defun eask-f-development (&rest dep)
  "Development scope with list of DEP."
  (setq dep (cl-remove-if #'null dep))  ; make sure no `nil', see #143
  (dolist (pkg dep)
    (push pkg eask-depends-on-dev)
    (delete-dups eask-depends-on-dev)
    (setq eask-depends-on (remove pkg eask-depends-on))))

(defun eask-f-exec-paths (&rest dirs)
  "Add all DIRS to the variable `exec-path'."
  (dolist (dir dirs) (add-to-list 'exec-path (expand-file-name dir) t)))

(defun eask-f-load-paths (&rest dirs)
  "Add all DIRS to to the variable `load-path'."
  (dolist (dir dirs) (add-to-list 'load-path (expand-file-name dir) t)))

;;
;;; Verbosity

(defcustom eask-verbosity 3
  "Log level for all messages; 4 means trace most anything, 0 means nothing.

Standard is, 0 (error), 1 (warning), 2 (info), 3 (log), 4 (debug), 5 (all)."
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

(defcustom eask-level-color
  '((all   . ansi-magenta)
    (debug . ansi-blue)
    (log   . ansi-white)
    (info  . ansi-cyan)
    (warn  . ansi-yellow)
    (error . ansi-red))
  "Alist of each log level's color, in (SYMBOL . ANSI-FUNCTION)."
  :type 'alist
  :group 'eask)

(defun eask--verb2lvl (symbol)
  "Convert verbosity SYMBOL to level."
  (cl-case symbol
    (`all   5)
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
  "Define verbosity scope.

Execute forms BODY limit by the verbosity level (SYMBOL)."
  (declare (indent 1) (debug t))
  `(if (eask--reach-verbosity-p ,symbol) (progn ,@body)
     (eask--silent ,@body)))

(defun eask--ansi (symbol string)
  "Paint STRING with color defined by log level (SYMBOL)."
  (if-let ((ansi-function (cdr (assq symbol eask-level-color))))
      (funcall ansi-function string)
    string))

(defun eask--format (prefix fmt &rest args)
  "Format Eask messages.

Argument PREFIX is a string identify the type of this messages.  Arguments FMT
and ARGS are used to pass through function `format'."
  (apply #'format
         (concat (when eask-timestamps (format-time-string "%Y-%m-%d %H:%M:%S "))
                 (when eask-log-level (concat prefix " "))
                 fmt)
         args))

(defun eask--msg (symbol prefix msg &rest args)
  "If level (SYMBOL) is at or below `eask-verbosity'; then, log the message.

For arguments PREFIX, MSG and ARGS, please see funtion `eask--format' for the
detials."
  (eask-with-verbosity symbol
    (let* ((string (apply #'eask--format prefix msg args))
           (output (eask--ansi symbol string))
           (output (eask--msg-displayable-kwds output))  ; Don't color, but replace it!
           (func (cl-case symbol
                   ((or error warn) symbol)
                   (t #'message))))
      (funcall func "%s" output))))

(defun eask-debug (msg &rest args)
  "Send debug message; see function `eask--msg' for arguments MSG and ARGS."
  (apply #'eask--msg 'debug "[DEBUG]" msg args))
(defun eask-log (msg &rest args)
  "Send log message; see function `eask--msg' for arguments MSG and ARGS."
  (apply #'eask--msg 'log   "[LOG]" msg args))
(defun eask-info (msg &rest args)
  "Send info message; see function `eask--msg' for arguments MSG and ARGS."
  (apply #'eask--msg 'info  "[INFO]" msg args))
(defun eask-warn (msg &rest args)
  "Send warn message; see function `eask--msg' for arguments MSG and ARGS."
  (apply #'eask--msg 'warn  "[WARNING]" msg args))
(defun eask-error (msg &rest args)
  "Send error message; see function `eask--msg' for arguments MSG and ARGS."
  (apply #'eask--msg 'error "[ERROR]" msg args))

(defun eask--msg-paint-kwds (string)
  "Paint keywords from STRING."
  (let* ((string (eask-s-replace "✓" (ansi-green "✓") string))
         (string (eask-s-replace "✗" (ansi-red "✗") string))
         (string (eask-s-replace "💡" (ansi-yellow "💡") string)))
    string))

(defun eask--msg-char-displayable (char replacement string)
  "Ensure CHAR is displayable in STRING; if not, we fallback to REPLACEMENT
character."
  (if (char-displayable-p (string-to-char char))
      string
    (eask-s-replace char replacement string)))

(defun eask--msg-displayable-kwds (string)
  "Make sure all keywords is displayable in STRING."
  (let* ((string (eask--msg-char-displayable "✓" "v" string))
         (string (eask--msg-char-displayable "✗" "X" string))
         (string (eask--msg-char-displayable "💡" "<?>" string)))
    string))

(defun eask--format-paint-kwds (msg &rest args)
  "Paint keywords after format MSG and ARGS."
  (let* ((string (apply #'format msg args))
         (string (eask--msg-paint-kwds string))
         (string (eask--msg-displayable-kwds string)))
    string))

(defun eask-msg (msg &rest args)
  "Like function `message' but replace unicodes with color.

For arguments MSG and ARGS, please see function `eask--format-paint-kwds' for
the detials."
  (message (apply #'eask--format-paint-kwds msg args)))

(defun eask-write (msg &rest args)
  "Like function `eask-msg' but without newline at the end.

For arguments MSG and ARGS, please see function `eask--format-paint-kwds' for
the detials."
  (unless inhibit-message
    (princ (apply #'eask--format-paint-kwds msg args) 'external-debugging-output)))

(defun eask-report (&rest args)
  "Report error/warning depends on strict flag.

Argument ARGS are direct arguments for functions `eask-error' or `eask-warn'."
  (apply (if (eask-strict-p) #'eask-error #'eask-warn) args))

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
  "On error.

Arguments FNC and ARGS are used for advice `:around'."
  (let ((msg (eask--ansi 'error (apply #'format-message args))))
    (unless eask--ignore-error-p
      (eask--unsilent (eask-msg "%s" msg)))
    (run-hook-with-args 'eask-on-error-hook 'error msg)
    (eask--trigger-error))
  (when debug-on-error (apply fnc args)))

(advice-add 'error :around #'eask--error)

(defun eask--warn (fnc &rest args)
  "On warn.

Arguments FNC and ARGS are used for advice `:around'."
  (let ((msg (eask--ansi 'warn (apply #'format-message args))))
    (unless eask--ignore-error-p
      (eask--unsilent (eask-msg "%s" msg)))
    (run-hook-with-args 'eask-on-warning-hook 'warn msg))
  (eask--silent (apply fnc args)))

(advice-add 'warn :around #'eask--warn)

;;
;;; Log

(defconst eask-log-path ".log"
  "Directory path to create log files.")

(defcustom eask-log-file nil
  "Weather to generate log files."
  :type 'boolean
  :group 'eask)

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
          (ignore-errors  ; The new files spec will trigger error, wrap it
            (package-build-expand-files-spec nil nil default-directory specs))))

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

(defun eask-package-multi-p ()
  "Return t if multi-files package."
  (or (bound-and-true-p package-build-build-function)
      (< 1 (length (eask-package-files)))))

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
  :type 'boolean
  :group 'eask)

(defcustom eask-minimum-reported-time 0.1
  "Minimal load time that will be reported."
  :type 'number
  :group 'eask)

(defmacro eask-with-progress (msg-start body msg-end)
  "Progress BODY wrapper with prefix (MSG-START) and suffix (MSG-END) messages."
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
  "Shorthand to progress SEQUENCE of task.

Arguments PREFIX and SUFFIX are strings to print before and after each progress.
Argument FUNC are execution for eash progress; this is generally the actual
task work."
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
  "Loop through each line and print each line with corresponds log level.

You can pass BUFFER-OR-NAME to replace current buffer."
  (with-current-buffer (or buffer-or-name (current-buffer))
    (goto-char (point-min))
    (while (not (eobp))
      (let ((line (buffer-substring-no-properties (line-beginning-position) (line-end-position))))
        (cond ((string-match-p "[: ][Ee]rror: " line) (eask-error line))
              ((string-match-p "[: ][Ww]arning: " line) (eask-warn line))
              (t (eask-log line))))
      (forward-line 1))))

(defun eask-delete-file (filename)
  "Delete a FILENAME from disk."
  (let (deleted)
    (eask-with-progress
      (format "Deleting %s... " filename)
      (eask-with-verbosity 'log
        (setq deleted (file-exists-p filename))
        (ignore-errors (delete-file filename))
        (setq deleted (and deleted (not (file-exists-p filename)))))
      (if deleted "done ✓" "skipped ✗"))
    deleted))

;;
;;; Help

(defun eask--help-display ()
  "Display help instruction."
  (goto-char (point-min))
  (let ((max-column 0))
    (while (not (eobp))
      (forward-line 1)
      (beginning-of-line)
      (insert "    ")
      (end-of-line)
      (setq max-column (max (current-column) max-column)))
    (eask-msg (concat "''" (spaces-string max-column) "''"))
    (eask-msg (ansi-white (buffer-string)))
    (eask-msg (concat "''" (spaces-string max-column) "''"))))

(defun eask-help (command)
  "Show COMMAND's help instruction."
  (let* ((command (eask-2str command))  ; convert to string
         (help-file (concat eask-lisp-root "help/" command)))
    (if (file-exists-p help-file)
        (with-temp-buffer
          (insert-file-contents help-file)
          (unless (string-empty-p (buffer-string))
            (let ((buf-str (eask--msg-displayable-kwds (buffer-string))))
              (erase-buffer)
              (insert buf-str))
            (eask--help-display)))
      (eask-error "Help manual missig %s" help-file))))

;;
;;; Checker

(defun eask--checker-existence ()
  "Return errors if required metadata is missing."
  (unless eask-package (eask-error "Missing metadata package; make sure you have create Eask-file with $ eask init!")))

(defun eask--check-strings (fmt f p &rest args)
  "Test strings (F and P); then print FMT and ARGS if not equal."
  (unless (string= f p) (apply #'eask-warn (append (list fmt f p) args))))

(defun eask--check-optional (f p msg1 msg2 msg3 msg4)
  "Conditional way to check optional headers, URL and KEYWORDS.

For arguments F and P, please see function `eask--check-strings' for more
information.

Arguments MSG1, MSG2, MSG3 and MSG4 are conditional messages."
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
       (format "Unmatched website URL '%s'; add %s to %s" eask-website-url
               (if (string-prefix-p "-pkg.el" def-point)
                   (format ":url \"%s\"" eask-website-url)
                 (format ";; URL: %s" eask-website-url))
               def-point)
       (format "Unmatched website URL '%s'; add (website-url \"%s\") to Eask-file" url url)
       (format "URL header is optional, but it's often recommended")))
    (let ((keywords (eask-package-desc-keywords)))
      (cond
       ((or keywords eask-keywords)
        (dolist (keyword keywords)
          (unless (member keyword eask-keywords)
            (eask-warn "Unmatched keyword '%s'; add (keywords \"%s\") to Eask-file or consider removing it" keyword keyword)))
        (dolist (keyword eask-keywords)
          (unless (member keyword keywords)
            (eask-warn "Unmatched keyword '%s'; add %s to %s or consider removing it"
                       keyword
                       (if (string-prefix-p "-pkg.el" def-point)
                           (format ":keywords '(\"%s\")" keyword)
                         (format ";; Keywords: %s" keyword))
                       def-point))))
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
  "Run checker for package's metadata.

Argument NAME represent the name of that package's metadata.  VAR is the actual
variable we use to test validation."
  (unless (stringp var)
    (eask-error "%s must be a string" name))
  (when (string-empty-p var)
    (eask-warn "%s cannot be an empty string" name)))

;;
;;; User customization

(defcustom eask-file-loaded-hook nil
  "Hook runs after Easkfile is loaded."
  :type 'hook
  :group 'eask)

(defcustom eask-before-command-hook nil
  "Hook runs before command is executed."
  :type 'hook
  :group 'eask)

(defcustom eask-after-command-hook nil
  "Hook runs after command is executed."
  :type 'hook
  :group 'eask)

(defcustom eask-on-error-hook nil
  "Hook runs when error is triggered."
  :type 'hook
  :group 'eask)

(defcustom eask-on-warning-hook nil
  "Hook runs when warning is triggered."
  :type 'hook
  :group 'eask)

(defcustom eask-dist-path "dist"
  "Name of default target directory for building packages."
  :type 'string
  :group 'eask)

;;
;;; Linter

(defvar eask-lint-first-file-p nil
  "Set the flag to t after the first file is linted.")

(defun eask-lint-first-newline ()
  "Built-in linters will create extra newline, prevent that!"
  (when eask-lint-first-file-p
    (eask-msg ""))
  (setq eask-lint-first-file-p t))

;;
;;; Externals

(eask-load "extern/compat")
(eask-load "extern/ansi")
(eask-load "extern/package")
(eask-load "extern/package-build")

;;
;;; Requirement

(when (version< emacs-version "26.1")
  (eask-error "Eask requires Emacs 26.1 and above!"))

;;; _prepare.el ends here

;;; init/cask.el --- Initialize Eask from Cask  -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; Commmand use to convert Cask-file to Eask-file
;;
;;   $ eask init --from cask
;;

;;; Code:

(let ((dir (file-name-directory (nth 1 (member "-scriptload" command-line-args)))))
  (load (expand-file-name "_prepare.el"
                          (locate-dominating-file dir "_prepare.el"))
        nil t))

;;
;;; Externals

(declare-function ansi-green "ext:ansi.el")
(declare-function ansi-yellow "ext:ansi.el")
(declare-function ansi-white "ext:ansi.el")

(declare-function -flatten "ext:dash.el")

(declare-function cask--read "ext:cask.el")

;;
;;; Core

(defvar eask--cask-contents nil
  "Store Cask-file contents.")

(defun eask--cask-filter-contents (name &optional contents)
  "Filter directives by NAME.

Optional argument CONTENTS is used for nested directives.  e.g. development."
  (cl-remove-if-not (lambda (prop)
                      (eq (car prop) name))
                    (or contents eask--cask-contents)))

(defun eask--cask-package-name ()
  "Return package name from Cask-file."
  (nth 0 (alist-get 'package eask--cask-contents)))

(defun eask--cask-package-version ()
  "Return package version from Cask-file."
  (nth 1 (alist-get 'package eask--cask-contents)))

(defun eask--cask-package-description ()
  "Return package description from Cask-file."
  (nth 2 (alist-get 'package eask--cask-contents)))

(defun eask--cask-package-file ()
  "Return package file from Cask-file."
  (car (alist-get 'package-file eask--cask-contents)))

(defun eask--cask-files ()
  "Return files from Cask-file."
  (alist-get 'files eask--cask-contents))

(defun eask--cask-package-descriptor ()
  "Return package descriptor from Cask-file."
  (car (alist-get 'package-descriptor eask--cask-contents)))

(defun eask--cask-sources ()
  "Return sources from Cask-file."
  (eask--cask-filter-contents 'source))

(defun eask--cask-reqs ()
  "Return dependencies from Cask-file."
  (eask--cask-filter-contents 'depends-on))

(defun eask--cask-reqs-dev ()
  "Return development dependencies file from Cask-file."
  (let ((dev-scopes (eask--cask-filter-contents 'development))
        (deps))
    (dolist (dev-scope dev-scopes)
      (setq deps (append deps (eask--cask-filter-contents 'depends-on (cdr dev-scope)))))
    deps))

(defun eask--cask-remove-emacs-dep (list)
  "Remove dependency Emacs from dependencies LIST."
  (cl-remove-if (lambda (req)
                  (when (string= (cadr req) "emacs")
                    (caddr req)))
                list))

(defun eask--cask-reqs-no-emacs ()
  "Return dependencies from Cask-file but exclude Emacs."
  (eask--cask-remove-emacs-dep (eask--cask-reqs)))

(defun eask--cask-reqs-dev-no-emacs ()
  "Return development dependencies from Cask-file but exclude Emacs."
  (eask--cask-remove-emacs-dep (eask--cask-reqs-dev)))

(defun eask--cask-emacs-version ()
  "Return Emacs version from Cask-file."
  (let ((reqs (eask--cask-reqs)))
    (cl-some (lambda (req)
               (when (string= (cadr req) "emacs")
                 (caddr req)))
             reqs)))

(defun eask--convert-cask (filename)
  "Convert Cask FILENAME to Eask."
  (let* ((filename (expand-file-name filename))
         (file (file-name-nondirectory (eask-root-del filename)))
         (new-file (eask-s-replace "Cask" "Eask" file))
         (new-filename (expand-file-name new-file))
         (eask--cask-contents (ignore-errors (cask--read filename)))  ; Read it!
         (converted))
    (eask-with-progress
      (format "Converting file `%s` to `%s`... " file new-file)
      (eask-with-verbosity 'debug
        (cond ((not (string-prefix-p "Cask" file))
               (eask-debug "✗ Invalid Cask filename, the file should start with `Cask`"))
              (t
               (with-current-buffer (find-file new-filename)
                 (erase-buffer)
                 (goto-char (point-min))

                 ;; XXX: Newline to look nicer!
                 (eask--unsilent (eask-msg "\n"))

                 (let* ((project-name
                         (or (eask--cask-package-name)
                             (file-name-nondirectory (directory-file-name default-directory))))
                        (package-name
                         (or (eask--cask-package-name)
                             (eask-read-string (format "package name: (%s) " project-name) nil nil project-name)))
                        (version (or (eask--cask-package-version)
                                     (eask-read-string "version: (1.0.0) " nil nil "1.0.0")))
                        (description (or (eask--cask-package-description)
                                         (eask-read-string "description: ")))
                        (guess-entry-point (eask-guess-entry-point project-name))
                        (entry-point
                         (or (eask--cask-package-file)
                             (eask-read-string (format "entry point: (%s) " guess-entry-point)
                                               nil nil guess-entry-point)))
                        (emacs-version
                         (or (eask--cask-emacs-version)
                             (eask-read-string "emacs version: (26.1) " nil nil "26.1")))
                        (website (eask-read-string "website: "))
                        (keywords (eask-read-string "keywords: "))
                        (keywords (split-string keywords "[, ]"))
                        (keywords (string-join keywords "\" \""))
                        (content (format
                                  "(package \"%s\"
         \"%s\"
         \"%s\")

(website-url \"%s\")
(keywords \"%s\")

(package-file \"%s\")
"
                                  package-name version description website keywords
                                  entry-point)))
                   (insert content)

                   (when-let* ((files (eask--cask-files))
                               (files (if (stringp files) files (-flatten files))))
                     (insert "(files")
                     (dolist (file files)
                       (if (stringp file)
                           (insert "\n \"" file "\"")
                         (insert "\n " (eask-2str file))))
                     (insert ")\n"))

                   (when-let* ((pkg-desc (eask--cask-package-descriptor)))
                     (insert "\n")
                     (insert "(package-descriptor \"" (eask-2str pkg-desc) "\")\n"))

                   (insert "\n")
                   (insert "(script \"test\" \"echo \\\"Error: no test specified\\\" && exit 1\")\n")

                   (when-let* ((sources (eask--cask-sources)))
                     (insert "\n")
                     (dolist (source sources)
                       (insert "(source '" (eask-2str (cadr source)) ")\n")))

                   (insert "\n")
                   (insert "(depends-on \"emacs\" \"" emacs-version "\")")
                   (unless (eask--cask-reqs-no-emacs)
                     (insert "\n"))  ; Make sure end line exists!

                   (when-let* ((pkgs (eask--cask-reqs-no-emacs)))
                     (insert "\n")
                     (dolist (pkg pkgs)
                       (let ((val (mapconcat #'eask-2str (cdr pkg) "\" \"")))
                         (insert "(depends-on \"" val "\")\n"))))

                   (when-let* ((pkgs (eask--cask-reqs-dev-no-emacs)))
                     (insert "\n")
                     (insert "(development\n")
                     (dolist (pkg pkgs)
                       (let ((val (mapconcat #'eask-2str (cdr pkg) "\" \"")))
                         (insert " (depends-on \"" val "\")\n")))
                     (insert " )\n")))

                 (save-buffer))
               (setq converted t))))
      (if converted "done ✓" "skipped ✗"))
    converted))

(eask-start
  ;; Preparation
  (eask-archive-install-packages '("gnu" "melpa" "jcs-elpa")
                                 '(package-build cask))

  ;; Start Converting
  (require 'cask)
  (let* ((patterns (eask-args))
         (files (if patterns
                    (eask-expand-file-specs patterns)
                  (directory-files default-directory t "Cask")))
         (converted 0))
    (cond
     ;; Files found, do the action!
     (files
      (dolist (file files)
        (when (eask--convert-cask file)
          (cl-incf converted)))
      (eask-msg "")
      (eask-info "(Total of %s Cask-file%s converted)" converted
                 (eask--sinr converted "" "s")))
     ;; Pattern defined, but no file found!
     (patterns
      (eask-info "(No files match wildcard: %s)"
                 (mapconcat #'identity patterns " ")))
     ;; Default, print help!
     (t
      (eask-info "(No Cask-files have been converted to Eask)")
      (eask-help "init/cask")))))

;;; init/cask.el ends here

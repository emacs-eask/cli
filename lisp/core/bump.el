;;; core/bump.el --- Bump version for your project  -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; Command use to bump version,
;;
;;   $ eask bump
;;
;;
;;  Positionals:
;;
;;    [levels..]     version level to bump; accept `major', `minor' or `patch'
;;

;;; Code:

(let ((dir (file-name-directory (nth 1 (member "-scriptload" command-line-args)))))
  (load (expand-file-name "_prepare.el"
                          (locate-dominating-file dir "_prepare.el"))
        nil t))

(defun eask-bump-version--version (version index)
  "Bump VERSION with INDEX."
  (let ((lst (if (stringp version) (version-to-list version) version)))
    (setf (nth index lst) (cl-incf (nth index lst)))
    (mapconcat #'eask-2str lst version-separator)))

(defun eask-bump-version--major-version (version)
  "Bump VERSION major level."
  (eask-bump-version--version version 0))

(defun eask-bump-version--minor-version (version)
  "Bump VERSION minor level."
  (eask-bump-version--version version 1))

(defun eask-bump-version--patch-level (version)
  "Bump VERSION patch level."
  (eask-bump-version--version version 2))

(eask-start
  (let ((package-file (or eask-package-file
                          (eask-guess-entry-point)))
        (levels (eask-args))
        (desc)
        (version)
        (tasks (if eask-file 2 1)))
    (unless eask-package-file
      (eask-info "💡 Detect package file `%s'..." package-file))
    (cond
     ((and (not (member "major" levels))
           (not (member "minor" levels))
           (not (member "patch" levels)))
      (eask-help "core/bump"))
     ((not (file-exists-p package-file))
      (eask-info "(No package file found)"))
     (t
      (with-current-buffer (find-file package-file)
        (setq desc (package-buffer-info)))
      (setq version (package-desc-version desc))
      (when (member "major" levels)
        (setq version (eask-bump-version--major-version version)))
      (when (member "minor" levels)
        (setq version (eask-bump-version--minor-version version)))
      (when (member "patch" levels)
        (setq version (eask-bump-version--patch-level version)))
      (eask-msg "New version: %s" version)
      (let (success)
        (eask-with-progress
          (format "  - [1/%s] Bump version for package-file (%s)... "
                  tasks
                  (eask-root-del package-file))
          (with-current-buffer (find-file package-file)
            (goto-char (point-min))
            (cond ((re-search-forward ";;[ \t]*Version:[ \t]*" nil t)
                   (delete-region (point) (line-end-position))
                   (insert version)
                   (setq success t)
                   (save-buffer))
                  (t
                   (eask-error
                    "Failed to bump version to package file; invalid search string: %s"
                    package-file))))
          (if success "done ✓" "skipped ✗")))
      (when eask-file
        (let (success)
          (eask-with-progress
            (format "  - [1/%s] Bump version for Eask-file (%s)... "
                    tasks
                    (eask-root-del eask-file))
            (with-current-buffer (find-file eask-file)
              (goto-char (point-min))
              (cond ((re-search-forward "(package[ \t\r\n\"]*" nil t)
                     (forward-char -1)
                     (forward-thing 'sexp)
                     (forward-thing 'sexp)
                     (forward-sexp -1)
                     (message "pt: %s" (point))
                     (delete-region (1+ (point)) (save-excursion
                                                   (forward-thing 'sexp)
                                                   (1- (point))))
                     (forward-char 1)
                     (insert version)
                     (setq success t)
                     (save-buffer))
                    (t
                     (eask-error
                      "Failed to bump version to Eask-file; invalid search string: %s"
                      eask-file))))
            (if success "done ✓" "skipped ✗"))))))))

;;; core/bump.el ends here

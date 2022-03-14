;;; upgrade.el --- Upgrade packages  -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; Command use to upgrade Emacs packages,
;;
;;   $ eask upgrade [name] [-g]
;;
;; If package [name] is specify; upgrade that specific package. Otherwise, we
;; upgrade all packages to target workspace.
;;
;; If [-g] is specify; we uprade package to `~/.emacs.d/'.
;;

;;; Code:

(load-file "./lisp/_prepare.el")

(defun eask-package-version (name where)
  "Get version of the package by NAME.

Argument WHERE is the alist of package information."
  (when-let ((pkg (cadr (assq name where))))
    (package-desc-version pkg)))

(defun eask-package-upgrade (pkg-desc)
  "Upgrade package using PKG-DESC."
  (let ((old-pkg (cadr (assq (package-desc-name pkg-desc) package-alist))))
    (package-install pkg-desc)
    (package-delete old-pkg)))

(defun eask-package--upgradable-p (pkg)
  "Return non-nil if PKG can be upgraded."
  (let ((current (eask-package-version pkg package-alist))
        (latest (eask-package-version pkg package-archive-contents)))
    (version-list-< current latest)))

(defun eask-package--upgrades ()
  "Return a list of upgradable package description."
  (let (upgrades)
    (dolist (pkg (mapcar #'car package-alist))
      (when (eask-package--upgradable-p pkg)
        (push (cadr (assq pkg package-archive-contents)) upgrades)))
    upgrades))

(defun eask-package-upgrade-all ()
  "Upgrade for archive packages."
  (if-let ((upgrades (eask-package--upgrades)))
      (progn
        (dolist (pkg-desc upgrades) (eask-package-upgrade pkg-desc))
        (message "Done upgrading all packages"))
    (message "All packages are up to date")))

(eask-start
  (package-refresh-contents)

  (if-let* ((name (elt argv 0)) (name (intern name)))
      (if (package-installed-p name)
          (if (or (eask-package--upgradable-p name) (eask-force-p))
              (eask-package-upgrade name)
            (message "Package `%s` is already up to date" name))
        (error "Package does not exists `%s`, you need to install before upgrade" name))
    (eask-package-upgrade-all)))

;;; upgrade.el ends here

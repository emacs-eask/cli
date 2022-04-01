;;; upgrade.el --- Upgrade packages  -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; Command use to upgrade Emacs packages,
;;
;;   $ eask upgrade [names..]
;;
;;
;;  Initialization options:
;;
;;    [names..]     package to upgrade; else we upgrade all packages
;;

;;; Code:

(load (expand-file-name
       "_prepare.el"
       (file-name-directory (nth 1 (member "-scriptload" command-line-args))))
      nil t)

(defun eask-package-upgrade (pkg-desc)
  "Upgrade package using PKG-DESC."
  (let ((force (eask-force-p))
        (old-pkg (cadr (assq (package-desc-name pkg-desc) package-alist))))
    (when force (package-delete old-pkg))
    (package-install pkg-desc)
    (unless force (package-delete old-pkg))))

(defun eask-package--upgradable-p (pkg)
  "Return non-nil if PKG can be upgraded."
  (let ((current (eask-package-version pkg t))
        (latest (eask-package-version pkg nil)))
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
        (eask-info "(Done upgrading all packages)"))
    (eask-info "(All packages are up to date)")))

(eask-start
  (eask-pkg-init)
  (if-let ((names (eask-args)))
      (dolist (name names)
        (setq name (intern name))
        (if (package-installed-p name)
            (if (or (eask-package--upgradable-p name) (eask-force-p))
                (eask-package-upgrade (cadr (assq name package-archive-contents)))
              (eask-warn "Package `%s` is already up to date" name))
          (eask-error "Package does not exists `%s`, you need to install before upgrade" name)))
    (eask-package-upgrade-all)))

;;; upgrade.el ends here

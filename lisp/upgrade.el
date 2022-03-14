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

(eask-start
  (package-refresh-contents)

  (if-let* ((name (elt argv 0)) (name (intern name)))
      (if (package-installed-p name)
          ()
        (error ""))
    ))

;;; upgrade.el ends here

;; ========================================================================
;; $File: install.el $
;; $Date: 2022-03-13 21:37:28 $
;; $Revision: $
;; $Creator: Jen-Chieh Shen $
;; $Notice: See LICENSE.txt for modification and distribution information
;;                   Copyright © 2022 by Shen, Jen-Chieh $
;; ========================================================================

(load-file "./lisp/_prepare.el")

(eask-start
  (let ((name (elt argv 0)))
    (if name
        ;; If package <name> are specified, we try to install it
        (package-install (intern name))
      ;; Else we try to install package from the working directory
      (package-install-file (expand-file-name "./")))))

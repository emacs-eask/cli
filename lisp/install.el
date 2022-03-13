;; ========================================================================
;; $File: install.el $
;; $Date: 2022-03-13 21:37:28 $
;; $Revision: $
;; $Creator: Jen-Chieh Shen $
;; $Notice: See LICENSE.txt for modification and distribution information
;;                   Copyright © 2022 by Shen, Jen-Chieh $
;; ========================================================================

(require 'package)

(let ((global (or (member "-g" argv)
                  (member "--global" argv)))
      (name (elt argv 0)))
  (unless global  ; set it locally, else we ignore to respect default settings
    (setq user-emacs-directory (expand-file-name ".eask/")
          package-user-dir (expand-file-name "elpa" user-emacs-directory)))
  (if name (package-install (intern name))  ; install target <name> package
    (package-install-file (expand-file-name "./")))  ; install locally
  )

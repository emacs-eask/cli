;; ========================================================================
;; $File: prepare.el $
;; $Date: 2022-03-13 23:34:21 $
;; $Revision: $
;; $Creator: Jen-Chieh Shen $
;; $Notice: See LICENSE.txt for modification and distribution information
;;                   Copyright © 2022 by Shen, Jen-Chieh $
;; ========================================================================

(require 'package)

(defun eask-global-p ()
  "Return non-nil if the workspace is global."
  (or (member "-g" argv)
      (member "--global" argv)))

(defmacro eask-start (&rest body)
  "Execute BODY with workspace setup."
  (declare (indent 0) (debug t))
  `(progn
     (unless (eask-global-p)  ; set it locally, else we ignore to respect default settings
       (setq user-emacs-directory (expand-file-name ".eask/")
             package-user-dir (expand-file-name "elpa" user-emacs-directory)))
     (ignore-errors (make-directory package-user-dir t))
     ,@body))

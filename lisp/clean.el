;; ========================================================================
;; $File: clean.el $
;; $Date: 2022-03-14 03:05:33 $
;; $Revision: $
;; $Creator: Jen-Chieh Shen $
;; $Notice: See LICENSE.txt for modification and distribution information
;;                   Copyright © 2022 by Shen, Jen-Chieh $
;; ========================================================================

(load-file "./lisp/_prepare.el")

(eask-start
  (ignore-errors (delete-directory user-emacs-directory t)))

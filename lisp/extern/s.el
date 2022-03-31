;;; s.el --- External module `s'  -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(defun s-replace (old new s)
  "Replaces OLD with NEW in S."
  (declare (pure t) (side-effect-free t))
  (replace-regexp-in-string (regexp-quote old) new s t t))

;;; s.el ends here

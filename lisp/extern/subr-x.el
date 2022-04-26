;;; subr-x.el --- External module `subr-x'  -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; This module is used to compatible with older Emacs version.
;;

;;; Code:

(eask-defvc< 26
  (defmacro if-let* (varlist then &rest else)
    "Bind variables according to VARLIST and evaluate THEN or ELSE.
This is like `if-let' but doesn't handle a VARLIST of the form
\(SYMBOL SOMETHING) specially."
    (declare (indent 2)
             (debug ((&rest [&or symbolp (symbolp form) (form)])
                     body)))
    (if varlist
        `(let* ,(setq varlist (internal--build-bindings varlist))
           (if ,(caar (last varlist))
               ,then
             ,@else))
      `(let* () ,then))))

;;; subr-x.el ends here

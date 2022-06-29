;;; package-recipe.el --- External module `package-recipe'  -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(require 'package-recipe nil t)

;; Specializations of package-build classes and methods to define a
;; directory based recipe.
(defclass package-directory-recipe (package-recipe)
  ((dir           :initarg :dir   :initform ".")))

(cl-defmethod package-recipe--working-tree ((rcp package-directory-recipe))
  (oref rcp dir))

(cl-defmethod package-build--get-commit ((_rcp package-directory-recipe)))

(cl-defmethod package-build--get-commit-time ((_rcp package-directory-recipe) _rev)
  (let ((now (current-time))) (logior (lsh (car now) 16) (cadr now))))

;;; package-recipe.el ends here

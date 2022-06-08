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

;; XXX: remove this after we drop 26.x
(eask-defvc< 27 (defun time-convert (time &rest _) 1654667087))

(cl-defmethod package-build--get-timestamp ((_rcp package-directory-recipe) _rev)
  (time-convert (current-time) 'integer))

;;; package-recipe.el ends here

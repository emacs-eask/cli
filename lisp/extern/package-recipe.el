;;; package-recipe.el --- External module `package-recipe'  -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(with-eval-after-load 'package-recipe
  ;; Specializations of package-build classes and methods to define a
  ;; directory based recipe.
  (defclass package-directory-recipe (package-recipe)
    ((dir           :initarg :dir   :initform ".")))

  (cl-defmethod package-recipe--working-tree ((rcp package-directory-recipe))
    (oref rcp dir))

  (cl-defmethod package-build--get-commit ((_rcp package-directory-recipe))))

;;; package-recipe.el ends here

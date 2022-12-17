;;; extern/package-recipe.el --- External module `package-recipe'  -*- lexical-binding: t; -*-
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

;; After https://github.com/melpa/package-build/commit/f032c806169b0fabbcac1eb44a4f1ae00674bfa8
(cl-defmethod package-build--get-commit-time ((_rcp package-directory-recipe) _rev)
  (eask-current-time))

;; After https://github.com/melpa/package-build/pull/67
(cl-defmethod package-build--checkout ((rcp package-directory-recipe)))

(cl-defmethod package-build--get-timestamp-version ((rcp package-directory-recipe))
  (package-build--get-commit-time rcp nil))

;;; extern/package-recipe.el ends here

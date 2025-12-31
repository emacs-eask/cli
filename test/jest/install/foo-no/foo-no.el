;;; foo-no.el --- foo -*- lexical-binding: t -*-

;; Copyright (C) 2025-2026  the Eask authors.

;; Author: none
;; Maintainer: none
;; URL: https://github.com/emacs-eask/cli/foo-no
;; Version: 0.0.1
;; Package-Requires: ((emacs "26.1"))
;; Keywords: test

;; This file is not part of GNU Emacs

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; commentary
;;

;;; Code:

(defun foo-no-message ()
  "docstring"
  (interactive "P")
  (message "Hello World!"))

(provide 'foo-no)
;;; foo-no.el ends here

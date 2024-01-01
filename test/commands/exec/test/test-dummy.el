;;; test-dummy.el --- Dummy buttercup test  -*- lexical-binding: t; -*-

;; Copyright (C) 2023  the Eask authors.

;; This file is not part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program. If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; Dummy buttercup test.
;;

;;; Code:

(describe "Dumm Test"
  (it "pattern 1"
    (let ((dummy-var-1 t))
      (expect dummy-var-1 :to-be t)))
  (it "pattern 2"
    (let ((dummy-var-2 nil))
      (expect dummy-var-2 :to-be nil))))

(provide 'test-dummy)
;;; test-dummy.el ends here

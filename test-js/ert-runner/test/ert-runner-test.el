;;; ert-runner-test.el --- Test the command ert-runner  -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2025  the Eask authors.

;; This program is free software; you can redistribute it and/or modify
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

;; Tests for the command ert-runner

;;; Code:

(require 'ert)
(require 'debug)

(ert-deftest ert-runner-test-1 ()
  (should (= 1 1)))

(provide 'ert-runner-test)
;;; ert-runner-test.el ends here

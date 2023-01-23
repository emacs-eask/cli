;;; mini.emacs.pkg.el --- Minimal test package  -*- lexical-binding: t; -*-

;; Copyright (C) 2022  Shen, Jen-Chieh
;; Created date 2022-03-29 01:52:58

;; Author: Shen, Jen-Chieh <jcs090218@gmail.com>
;; URL: https://github.com/emacs-eask/mini.emacs.pkg
;; Version: 0.0.1
;; Package-Requires: ((emacs "24.3") (s "1.12.0") (fringe-helper "1.0.1"))
;; Keywords: test

;; This file is NOT part of GNU Emacs.

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
;;
;; Minimal Emacs package to simulate development environment; only for testing
;; purposes!
;;

;;; Code:

(require 's)
(require 'fringe-helper)

(provide 'mini.emacs.pkg)
;;; mini.emacs.pkg.el ends here

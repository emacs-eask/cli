;;; make-outdate.el --- Make some packages outdate  -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2026  the Eask authors.

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
;; Make some packages outdate
;;

;;; Code:

(eask-pkg-init)

(require 'f)

(defconst make-outdate-version "20001122.1234"
  "Outdate version.")

(defun make-outdate-package (name)
  "Make package (NAME) outdate."
  (let* ((dir (file-name-directory (locate-library name)))
         (pkg (concat dir name "-pkg.el")))
    (with-current-buffer (find-file pkg)
      (goto-char (point-min))
      (when (re-search-forward "\"[0-9.]*\"" nil t)
        (save-excursion
          (let ((end (point)))
            (backward-sexp)
            (delete-region (point) end)
            (insert (format "\"%s\"" make-outdate-version)))))
      (save-buffer)
      (kill-current-buffer))
    (let ((dest (expand-file-name (concat name "-" make-outdate-version "/") package-user-dir)))
      (eask-info "Moving %s" dir)
      (eask-info "    to %s" dest)
      (ignore-errors (make-directory dest t))
      (f-copy-contents dir dest)
      (ignore-errors (delete-directory dir t)))))

(make-outdate-package "dash")
(make-outdate-package "f")

(provide 'make-outdate)
;;; make-outdate.el ends here

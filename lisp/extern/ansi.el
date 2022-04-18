;;; ansi.el --- Turn string into ansi strings  -*- lexical-binding: t; -*-

;; Copyright (C) 2010-2013 Johan Andersson

;; Author: Johan Andersson <johan.rejeep@gmail.com>
;; Maintainer: Johan Andersson <johan.rejeep@gmail.com>
;; Version: 0.4.1
;; Keywords: terminals color ansi
;; URL: http://github.com/rejeep/ansi
;; Package-Requires: ((emacs "24.1") (cl-lib "0.6"))

;; This file is NOT part of GNU Emacs.

;;; License:

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;; Turns simple strings to ansi strings.

;; Turning a string into an ansi string can be to add color to a
;; text, add color in the background of a text or adding a style,
;; such as bold, underscore or italic.

;;; Code:

(require 'cl-lib)

(defgroup ansi nil
  "Turn string into ansi strings."
  :group 'lisp)

(defcustom ansi-inhibit-ansi nil
  "If non-nil, no apply ANSI code.
This variable affects `with-ansi', `with-ansi-princ'."
  :group 'ansi
  :type 'boolean)



(defconst ansi-colors
  '((black   . 30)
    (red     . 31)
    (green   . 32)
    (yellow  . 33)
    (blue    . 34)
    (magenta . 35)
    (cyan    . 36)
    (white   . 37))
  "List of text colors.")

(defconst ansi-on-colors
  '((on-black   . 40)
    (on-red     . 41)
    (on-green   . 42)
    (on-yellow  . 43)
    (on-blue    . 44)
    (on-magenta . 45)
    (on-cyan    . 46)
    (on-white   . 47))
  "List of colors to draw text on.")

(defconst ansi-styles
  '((bold       . 1)
    (dark       . 2)
    (italic     . 3)
    (underscore . 4)
    (blink      . 5)
    (rapid      . 6)
    (contrary   . 7)
    (concealed  . 8)
    (strike     . 9))
  "List of styles.")

(defvar ansi-csis
  '((up       . "A")
    (down     . "B")
    (forward  . "C")
    (backward . "D"))
  "...")

(defconst ansi-reset 0 "Ansi code for reset.")



(defun ansi--concat (&rest sequences)
  "Concat string elements in SEQUENCES."
  (apply #'concat (cl-remove-if-not 'stringp sequences)))

(defun ansi--code (effect)
  "Return code for EFFECT."
  (or
   (cdr (assoc effect ansi-colors))
   (cdr (assoc effect ansi-on-colors))
   (cdr (assoc effect ansi-styles))))

(defun ansi--char (effect)
  "Return char for EFFECT."
  (cdr (assoc effect ansi-csis)))

(defmacro ansi--define (effect)
  "Define ansi function with EFFECT."
  (let ((fn-name (intern (format "ansi-%s" (symbol-name effect)))))
    `(defun ,fn-name (format-string &rest objects)
       ,(format "Add '%s' ansi effect to text." effect)
       (apply 'ansi-apply (cons ',effect (cons format-string objects))))))

(defmacro with-ansi (&rest body)
  "Shortcut names (without ansi- prefix) can be used in this BODY."
  (if ansi-inhibit-ansi
      `(ansi--concat ,@body)
    `(cl-macrolet
         ,(mapcar
           (lambda (alias)
             (let ((fn (intern (format "ansi-%s" (symbol-name alias)))))
               `(,alias (string &rest objects)
                        ,(list 'backquote (list fn ',string ',@objects)))))
           (append
             (mapcar 'car ansi-colors)
             (mapcar 'car ansi-on-colors)
             (mapcar 'car ansi-styles)
             (mapcar 'car ansi-csis)))
       ,(cons 'ansi--concat body))))

(defmacro with-ansi-princ (&rest body)
  "Shortcut names (without ansi- prefix) can be used in this BODY and princ."
  (if ansi-inhibit-ansi
      `(princ (ansi--concat ,@body))
    `(princ (with-ansi ,@body))))

(defun ansi-apply (effect-or-code format-string &rest objects)
  "Apply EFFECT-OR-CODE to text.
FORMAT-STRING and OBJECTS are processed same as `apply'."
  (let* ((format-string (if (stringp format-string) format-string
                          (format "%s" format-string)))
         (code (if (numberp effect-or-code)
                   effect-or-code
                 (ansi--code effect-or-code)))
         (text (apply 'format format-string objects)))
    (if ansi-inhibit-ansi text
      (format "\e[%dm%s\e[%sm" code text ansi-reset))))

(defun ansi-csi-apply (effect-or-char &optional reps)
  "Apply EFFECT-OR-CHAR REPS (1 default) number of times."
  (let ((char (if (symbolp effect-or-char)
                  (ansi--char effect-or-char)
                effect-or-char)))
    (format "\u001b[%d%s" (or reps 1) char)))

(defun ansi-up (&optional n)
  "Move N steps (1 step default) up."
  (ansi-csi-apply 'up n))

(defun ansi-down (&optional n)
  "Move N steps (1 step default) down."
  (ansi-csi-apply 'down n))

(defun ansi-forward (&optional n)
  "Move N steps (1 step default) forward."
  (ansi-csi-apply 'forward n))

(defun ansi-backward (&optional n)
  "Move N steps (1 step default) backward."
  (ansi-csi-apply 'backward n))




(ansi--define black)
(ansi--define red)
(ansi--define green)
(ansi--define yellow)
(ansi--define blue)
(ansi--define magenta)
(ansi--define cyan)
(ansi--define white)

(ansi--define on-black)
(ansi--define on-red)
(ansi--define on-green)
(ansi--define on-yellow)
(ansi--define on-blue)
(ansi--define on-magenta)
(ansi--define on-cyan)
(ansi--define on-white)

(ansi--define bold)
(ansi--define dark)
(ansi--define italic)
(ansi--define underscore)
(ansi--define blink)
(ansi--define rapid)
(ansi--define contrary)
(ansi--define concealed)
(ansi--define strike)

(provide 'ansi)

;;; ansi.el ends here

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

(defconst ansi-bright-colors
  '((bright-black   . 90)
    (bright-red     . 91)
    (bright-green   . 92)
    (bright-yellow  . 93)
    (bright-blue    . 94)
    (bright-magenta . 95)
    (bright-cyan    . 96)
    (bright-white   . 97))
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

(defconst ansi-on-bright-colors
  '((on-bright-black   . 100)
    (on-bright-red     . 101)
    (on-bright-green   . 102)
    (on-bright-yellow  . 103)
    (on-bright-blue    . 104)
    (on-bright-magenta . 105)
    (on-bright-cyan    . 106)
    (on-bright-white   . 107))
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
  '((up            . "A")
    (down          . "B")
    (forward       . "C")
    (backward      . "D")
    (next-line     . "E")
    (previous-line . "F")
    (column        . "G")
    (kill          . "K"))
  "CSI (Control Sequence Introducer) sequences")

(defconst ansi-reset 0 "Ansi code for reset.")



(defun ansi--concat (&rest sequences)
  "Concat string elements in SEQUENCES."
  (apply #'concat (cl-remove-if-not 'stringp sequences)))

(defun ansi--code (effect)
  "Return code for EFFECT."
  (or
   (cdr (assoc effect ansi-colors))
   (cdr (assoc effect ansi-bright-colors))
   (cdr (assoc effect ansi-on-colors))
   (cdr (assoc effect ansi-on-bright-colors))
   (cdr (assoc effect ansi-styles))))

(defun ansi--is-alias (effect)
  "Return non-nil if EFFECT is available in DSL."
  (or
   (car (assoc effect ansi-colors))
   (car (assoc effect ansi-bright-colors))
   (car (assoc effect ansi-on-colors))
   (car (assoc effect ansi-on-bright-colors))
   (car (assoc effect ansi-styles))
   (car (assoc effect ansi-csis))))

(defun ansi--char (effect)
  "Return char for EFFECT."
  (cdr (assoc effect ansi-csis)))

(defmacro ansi--define (effect)
  "Define ansi function with EFFECT."
  (let ((fn-name (intern (format "ansi-%s" (symbol-name effect)))))
    `(defun ,fn-name (format-string &rest objects)
       ,(format "Add '%s' ansi effect to text." effect)
       (apply 'ansi-apply ',effect format-string objects))))

(cl-eval-when (compile eval load)
  (defun ansi--substitute (body)
    (if (listp body)
        (if (ansi--is-alias (car body))
            `(,(intern (format "ansi-%s" (symbol-name (car body))))
              ,@(mapcar (lambda (x) (ansi--substitute x)) (cdr body)))
          (mapcar (lambda (x) (ansi--substitute x)) body))
      body)))

(defmacro with-ansi (&rest body)
  "Shortcut names (without ansi- prefix) can be used in this BODY."
  `(ansi--concat ,@(ansi--substitute (mapcar #'macroexpand-all body))))

(defmacro with-ansi-princ (&rest body)
  "Shortcut names (without ansi- prefix) can be used in this BODY and princ."
  `(princ (with-ansi ,@body)))

(defun ansi-apply (effect-or-code format-string &rest objects)
  "Apply EFFECT-OR-CODE to text.
FORMAT-STRING and OBJECTS are processed same as `apply'."
  (let ((code (if (numberp effect-or-code)
                  effect-or-code
                (ansi--code effect-or-code)))
        (text (apply 'format format-string objects)))
    (if ansi-inhibit-ansi
        text
      (format "\e[%dm%s\e[%sm" code text ansi-reset))))

(defun ansi-csi-apply (effect-or-char &optional reps)
  "Apply EFFECT-OR-CHAR REPS (1 default) number of times."
  (if ansi-inhibit-ansi ""
    (let ((char (if (symbolp effect-or-char)
                    (ansi--char effect-or-char)
                  effect-or-char)))
      (format "\u001b[%d%s" (or reps 1) char))))

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

(defun ansi-next-line (&optional n)
  "Moves cursor to beginning of the line N (default 1) lines down."
  (ansi-csi-apply 'next-line n))

(defun ansi-previous-line (&optional n)
  "Moves cursor to beginning of the line N (default 1) lines up."
  (ansi-csi-apply 'previous-line n))

(defun ansi-column (&optional n)
  "Moves the cursor to column N (default 1)"
  (ansi-csi-apply 'column n))

(defun ansi-kill (&optional n)
  "Erase part of the line.

If N is 0 (or missing), clear from cursor to the end of the line.

If N is 1, clear from cursor to beginning of the line.

If N is 2, clear entire line. Cursor position does not change."
  (ansi-csi-apply 'kill n))



(ansi--define black)
(ansi--define red)
(ansi--define green)
(ansi--define yellow)
(ansi--define blue)
(ansi--define magenta)
(ansi--define cyan)
(ansi--define white)

(ansi--define bright-black)
(ansi--define bright-red)
(ansi--define bright-green)
(ansi--define bright-yellow)
(ansi--define bright-blue)
(ansi--define bright-magenta)
(ansi--define bright-cyan)
(ansi--define bright-white)

(ansi--define on-black)
(ansi--define on-red)
(ansi--define on-green)
(ansi--define on-yellow)
(ansi--define on-blue)
(ansi--define on-magenta)
(ansi--define on-cyan)
(ansi--define on-white)

(ansi--define on-bright-black)
(ansi--define on-bright-red)
(ansi--define on-bright-green)
(ansi--define on-bright-yellow)
(ansi--define on-bright-blue)
(ansi--define on-bright-magenta)
(ansi--define on-bright-cyan)
(ansi--define on-bright-white)

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

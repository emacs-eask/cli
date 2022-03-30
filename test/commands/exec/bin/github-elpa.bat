#!/bin/sh
:;#-*-Emacs-Lisp-*-

:;test -n "$EMACS" || EMACS=emacs
:;test -n "$CASK_EMACS" && EMACS=$CASK_EMACS
:;exec "$EMACS" -batch -Q -l "$0" -- "$@"

;;; github-elpa --- Build and publish ELPA repositories with GitHub Pages

;; Copyright (C) 2016 10sr

;; Author: 10sr<8slashes+el@gmail.com>
;; URL: https://github.com/10sr/github-elpa

;; This file is not part of GNU Emacs.

;; This is free and unencumbered software released into the public domain.

;; Anyone is free to copy, modify, publish, use, compile, sell, or
;; distribute this software, either in source code form or as a compiled
;; binary, for any purpose, commercial or non-commercial, and by any
;; means.

;; In jurisdictions that recognize copyright laws, the author or authors
;; of this software dedicate any and all copyright interest in the
;; software to the public domain. We make this dedication for the benefit
;; of the public at large and to the detriment of our heirs and
;; successors. We intend this dedication to be an overt act of
;; relinquishment in perpetuity of all present and future rights to this
;; software under copyright law.

;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
;; EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
;; MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
;; IN NO EVENT SHALL THE AUTHORS BE LIABLE FOR ANY CLAIM, DAMAGES OR
;; OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE,
;; ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR
;; OTHER DEALINGS IN THE SOFTWARE.

;; For more information, please refer to <http://unlicense.org/>

;;; Commentary:

;; github-elpa is an Emacs command-line utility to build your own
;; package.el-compatible package repository in your git repository.
;; By default this repository will be built into docs/elpa directory,
;; so by just pushing it to GitHub you can publish the repository with
;; GitHub Pages.

;; This package is mainly intended to be used from Cask.
;; See https://github.com/10sr/github-elpa for usage guide.

;;; Code:

(require 'github-elpa)
(require 'commander)

(defun github-elpa-bin-update ()
  "Update elpa repository."
  (github-elpa-bin-build)
  (github-elpa-bin-commit))

(defun github-elpa-bin-build ()
  "Update elpa archives without committing them."
  (github-elpa-build))

(defun github-elpa-bin-commit ()
  "Commit elpa archives."
  (github-elpa-commit))

(defun github-elpa-bin-set-working-dir (arg)
  "Set directory in which to keep checkouts to ARG."
  (setq github-elpa-working-dir arg))

(defun github-elpa-bin-set-archive-dir (arg)
  "Set directory in which to keep compiled archives to ARG."
  (setq github-elpa-archive-dir arg))

(defun github-elpa-bin-set-recipes-dir (arg)
  "Set directory that contains recipe files to ARG."
  (setq github-elpa-recipes-dir arg))

(defun github-elpa-bin-set-tar-executable (arg)
  "Set tar executable name to ARG."
  (setq github-elpa-tar-executable arg))


(commander
 (name "github-elpa")
 (description "Your own ELPA archive in GitHub page")
 (default commander-print-usage-and-exit)
 (option "--help, -h" "Show usage information and exit" commander-print-usage-and-exit)

 (command "update" "Update elpa repository" github-elpa-bin-update)
 (command "build" "Update elpa archives without committing them"
          github-elpa-bin-build)
 (command "commit" "Commit elpa archives" github-elpa-bin-commmit)

 (option "--working-dir <working-dir>, -w <working-dir>"
         github-elpa-bin-set-working-dir)
 (option "--archive-dir <archive-dir>, -a <archive-dir>"
         github-elpa-bin-set-archive-dir)
 (option "--recipes-dir <recipes-dir>, -r <recipes-dir>"
         github-elpa-bin-set-recipes-dir)
 (option "--tar <tar-executable>, -t <tar-executable>"
         github-elpa-bin-set-tar-executable)
 )

;;; github-elpa ends here

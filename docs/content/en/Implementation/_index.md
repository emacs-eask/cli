---
title: Implementation
weight: 600
---

Eask consists of two components: a command-line tool (the Eask CLI),
and Elisp scripts.

The CLI, is used to find the corresponding lisp file and feed it into the
Emacs executable. It would parse all options and convert them to Emacs
understandable options on the lisp scripts end. It is written in plain
JavaScript, the main file is located in **src/util.js**.

The E scripts, is used to do the actual execution for each command that passes
through the CLI. All commands are split into its file and are organized in the
**lisp** folder. It is written in plain Emacs Lisp, the main file is located in
**lisp/_prepare.el**.

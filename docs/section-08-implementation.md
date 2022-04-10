---
title: Implementation
permalink: implementation
---

# Implementation

Eask consists of two components: a Lisp core (scripts), a command-line tool
(the Eask CLI).

The scripts, is used to do the actual execution for each command that passes
through the CLI. All commands are split into its file and are organized in the
**lisp** folder. It is written in plain Emacs Lisp, the main file is located in
**lisp/_prepare.el**.

The CLI, is used to find the corresponding lisp file and feed it into the
Emacs executable. It would parse all options and converted them to Emacs
understandable options on the lisp scripts end. Is is written in plain 
JavaScript, the main file is located in **src/util.js**.

## Project Structure

There are **three** places you need to look into it:

1. `eask` file at the root of the project
2. `cmds` folder with all available commands
3. `lisp` folder with all elisp code
  - `lisp/extern` is the external modules/packages we used

`eask` is the node entry, and the main yargs definition! `cmds` and `lisp`
folders are command files that correspond to each other.

## Execution Order for Eask

Eask is executed this way:

![](./assets/flowchart/execution order.png)

* **Eask environment** builds sandbox and reads Eask file information
* **Emacs configuration** is only being executed when `-g` option is enabled
* **before hooks** are hooks run before command task
* **command execution** is the primary command task
* **after hooks** are hooks run after command task

[![License: GPL v3](https://img.shields.io/badge/License-GPL%20v3-green.svg)](https://www.gnu.org/licenses/gpl-3.0)
[![Emacs Version](https://img.shields.io/badge/Emacs-26.1+-7F5AB6.svg?logo=gnu%20emacs&logoColor=white)](https://www.gnu.org/software/emacs/download.html)
[![Release](https://img.shields.io/github/release/emacs-eask/eask.svg?logo=github)](https://github.com/emacs-eask/eask/releases/latest)
[![npm](https://img.shields.io/npm/v/@emacs-eask/eask?logo=npm&color=green)](https://www.npmjs.com/package/@emacs-eask/eask)
[![npm-dm](https://img.shields.io/npm/dm/@emacs-eask/eask.svg)](https://npmcharts.com/compare/@emacs-eask/eask?minimal=true)

# eask
> A set of command-line tools to build Emacs packages

[![Compile](https://github.com/emacs-eask/eask/actions/workflows/test-redefine.yml/badge.svg)](https://github.com/emacs-eask/eask/actions/workflows/test-redefine.yml)
[![Commands](https://github.com/emacs-eask/eask/actions/workflows/test-commands.yml/badge.svg)](https://github.com/emacs-eask/eask/actions/workflows/test-commands.yml)
[![Exec](https://github.com/emacs-eask/eask/actions/workflows/test-exec.yml/badge.svg)](https://github.com/emacs-eask/eask/actions/workflows/test-exec.yml)
[![Error](https://github.com/emacs-eask/eask/actions/workflows/test-error.yml/badge.svg)](https://github.com/emacs-eask/eask/actions/workflows/test-error.yml)
[![Color](https://github.com/emacs-eask/eask/actions/workflows/test-color.yml/badge.svg)](https://github.com/emacs-eask/eask/actions/workflows/test-color.yml)

Eask is heavily inspired by Cask, so they are somewhat related! This tool focuses
on consistency! [Cask]() and [makem.sh]() both rely on bash which Windows doesn't
run on by default. If you use WSL or other environment system file Cygwin/MSYS
; then this may not be the tool you are looking for! 👀

## 📰 News

* `0.4.0` - Add color logger
* `0.3.0` - Add verbosity level and timestamps
* `0.2.0` - Done basic error handling with exit code at the end of executions
* `0.1.39` - Use `spawn` instead `exec`; now messages will be printed immediately
* `0.1.0` - Project barebones are pretty much complete!

## 🔗 Link

* [Documentation](https://emacs-eask.github.io/)

## 📂 Related Projects

* [cask](https://github.com/cask/cask) - Project management tool for Emacs
* [makem.sh](https://github.com/alphapapa/makem.sh) - Makefile-like script for building and testing Emacs Lisp packages

## 📂 State of the project

The project's barebones are pretty much done, we are currently looking for
contirbutors to give us feedback and improve our TUI/UX for this tool!

We are also looking for advice to add more Emacser often use commands and
options, so these features are prepared by default! Like command `lint` 
(package-lint) or option `--debug` refers to `debug-on-error` to `t`!

## 🏆 Goals for the project

1. Consistent sandbox testing across all systems
2. Has most commands that Emacsers often used (byte-compile, checkdoc, etc)
3. No dependencies installed by default; only install packages when it's needed!
4. Extra: Package Management Tool for your personal configuration/package

## 📂 Project Structure

The project structure are very simple, all we need is to look into 3 places.

1. `eask` file at the root of the project
2. `cmds` folder with all available commands
3. `lisp` folder with all elisp code
  - `lisp/extern` is the external modules/packages we used

`eask` is the node entry, and the main yargs definition! `cmds` and `lisp`
folders are command files that correspond to each other.

The sandbox logic is stored inside file `./lisp/_prepare.el`.

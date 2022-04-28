---
title: Introduction
---

Eask is a command-line tool that helps you build, lint, and test Emacs Lisp
packages. It creates a clean environment to sandbox your elisp code without
influencing your personal configuration. Eask aims to be:

* **Consistent** enough to sandbox across all systems
* **General** enough to have Emacsers often use commands (byte-compile, checkdoc, etc)
* **Robust** enough to provide useful results even in the presense of user's errors
* **Dependency-free** so that the tool can be run on any platform

## ❓ Why Eask?

Eask is heavily inspired by Cask, so they are somewhat related! This tool focuses
on consistency! [Cask](https://github.com/cask/cask) and [makem.sh](https://github.com/alphapapa/makem.sh)
both rely on bash which Windows doesn't run on by default. If you use WSL or other
environment system file Cygwin/MSYS ; then this may not be the tool you are looking
for! 👀

## 📰 News

* `0.5.0` - Handle error for failed archive
* `0.4.0` - Add color logger
* `0.3.0` - Add verbosity level and timestamps
* `0.2.0` - Done basic error handling with exit code at the end of executions
* `0.1.39` - Use `spawn` instead `exec`; now messages will be printed immediately
* `0.1.0` - Project barebones are pretty much complete!

## 📝 Todo list

### Development

- [ ] [DEV] Release with an executable, so we no longer have to rely on node!
- [ ] [BUG] Handle condition, package doesn't in archives

### Eask-file commands

- [ ] [FEAT] Add `add-source` command

### Linter

- [ ] [FEAT] Add `elisp-lint` command
- [ ] [FEAT] Add `elint` command
- [ ] [FEAT] Add `elsa` command
- [ ] [FEAT] Add `lint-declare` command
- [ ] [FEAT] Add `lint-regexps` command

## 📂 Underlying Projects

The design of Eask was greatly influenced by the following projects:

* [cask](https://github.com/cask/cask) - Project management tool for Emacs
* [makem.sh](https://github.com/alphapapa/makem.sh) - Makefile-like script for building and testing Emacs Lisp packages
* [epm](https://github.com/xuchunyang/epm) - Emacs Package Manager
* [eldev](https://github.com/doublep/eldev) - Elisp Development Tool

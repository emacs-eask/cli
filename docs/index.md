---
title: Introduction
---

# Introduction

Eask is a command-line tool that helps you build, lint, and test Emacs Lisp
packages. It creates a clean environment to sandbox your elisp code without
influencing your personal configuration. Eask aims to be:

* **Consistent** enough to sandbox across all systems
* **General** enough to have Emacsers often used commands (byte-compile, checkdoc, etc)
* **Robust** enought to provide useful results even in the presense of user's errors
* **Dependency-free** so that the tool can be run on any platform

## ❓ Why Eask?

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

## 📝 Todo list

- [ ] Handle condition, package doesn't in archives
- [ ] Add `reinstall` command
- [ ] Add `elisp-lint` command
- [ ] Add `elint` command
- [ ] Add `elsa` command
- [ ] Add `lint-declare` command
- [ ] Add `lint-indent` command
- [ ] Add `lint-regexps` command
- [ ] Add `add-source` command

## 📂 Underlying Projects

The design of Eask was greatly influenced by the following projects:

* [cask](https://github.com/cask/cask) - Project management tool for Emacs
* [makem.sh](https://github.com/alphapapa/makem.sh) - Makefile-like script for building and testing Emacs Lisp packages
* [epm](https://github.com/xuchunyang/epm) - Emacs Package Manager

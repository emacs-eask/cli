---
title: Introduction
---

# Introduction

Eask is heavily inspired by Cask, so they are somewhat related! This tool focuses
on consistency! [Cask]() and [makem.sh]() both rely on bash which Windows doesn't
run on by default. If you use WSL or other environment system file Cygwin/MSYS
; then this may not be the tool you are looking for! 👀

## Why Node.JS?

Node has a better support on all kind of terminal applications (compare to just
the shell script)! Like colorful interface, entire npm community, etc; so you
can build cross-platform software with fewer hassles! Especially, after Microsoft
had bought the NPM inc, and would likely to support their own system well.

Cask does not seem to support Windows (no WSL) after version `0.8.6`. In the
early versions, they have used Python, but due to the Python supports on Windows
are just not as good as Node.JS.

## Who should use this tool?

People who like to use Emacs on Windows (no WSL), and would like to keep their
Emacs configuration/packages consistent on every operating system!

## 📝 Todo list

- [ ] Add `elint` command
- [ ] Add `elsa` command
- [ ] Add `lint-declare` command
- [ ] Add `lint-indent` command
- [ ] Add `lint-regexps` command
- [ ] Add `add-source` command

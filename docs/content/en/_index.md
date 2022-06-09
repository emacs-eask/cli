---
title: Introduction
---

Eask is a command-line tool that helps you build, lint, and test Emacs Lisp
packages. It creates a clean environment to sandbox your elisp code without
influencing your personal configuration. Eask aims to be:

* **Consistent** enough to sandbox across all systems
* **General** enough to have Emacsers often use commands (byte-compile, checkdoc, etc)
* **Robust** enough to provide useful results even in the presence of user's errors
* **Dependency-free** so that the tool can be run on any platform

## ❓ Why Eask?

Eask is heavily inspired by Cask, so they are somewhat related! This tool focuses
on consistency! [Cask](https://github.com/cask/cask) and [makem.sh](https://github.com/alphapapa/makem.sh)
both rely on bash which Windows doesn't run on by default. If you use WSL or other
environment system file Cygwin/MSYS ; then this may not be the tool you are looking
for! 👀

## ⚖️ Comparisons

The table were compiled by reading these projects’ documentation and source code,
but the author is not an expert on these tools. Corrections are welcome.

|          | Behind technology                 | Cross-Platform                                                   | Emacs Version | Size                |
|----------|-----------------------------------|------------------------------------------------------------------|---------------|---------------------|
| Cask     | Bash, Batch, and Python (Windows) | ❌ Good on Linux and macOS, but it's particularly bad on Windows | 24.5+         | 3,000+ lines        |
| makem.sh | Shellscript                       | ❌ Doesn't work on Windows by default                            | 26.1+         | 1 file, 1200+ lines |
| Eldev    | Bash, Batch, and Powershel, etc   | ✔ Good, but qutie slow on Windows                                | 24.4+         | 4,000+ lines        |
| Eask     | Node or Native Executables        | ✔ Good, and it can be compiled to native executables             | 26.1+         | 3,000+ lines        |

{{< hint info >}}
💡 **makem.sh** has a good comparisons document as well, visit their [site](https://github.com/alphapapa/makem.sh#comparisons)
{{< /hint >}}

## 📰 News

* `0.7.x` - Fix `default-directory` isn't honored by **-g** option
* `0.6.x` - You can now use `eask create` to create an Elisp project
* `0.5.x` - Handle error for failed archive
* `0.4.x` - Add color logger
* `0.3.x` - Add verbosity level and timestamps
* `0.2.x` - Done basic error handling with exit code at the end of executions
* `0.1.39` - Use `spawn` instead `exec`; now messages will be printed immediately
* `0.1.x` - Project bare-bones are pretty much complete!

## 📝 Todo list

### Development

- [ ] [DEV] Publish package to [homebrew]()
- [ ] [DEV] Publish package to [Chocolatety]()
- [ ] [DEV] Publish package to [Scoop]()
- [ ] [DEV] Publish package to [MacPorts]()

### Core commands

- [ ] [FEAT] Add `publish` command; to publish package to eask archive?

### Eask-file commands

- [ ] [FEAT] Add `add-source` command

### Linter

- [ ] [FEAT] Add `elisp-lint` command

### Testing

- [ ] [FEAT] Add `ecukes` command

## 📂 Underlying Projects

The design of Eask was greatly influenced by the following projects:

* [cask](https://github.com/cask/cask) - Project management tool for Emacs
* [makem.sh](https://github.com/alphapapa/makem.sh) - Makefile-like script for building and testing Emacs Lisp packages
* [epm](https://github.com/xuchunyang/epm) - Emacs Package Manager
* [eldev](https://github.com/doublep/eldev) - Elisp Development Tool

[![License: GPL v3](https://img.shields.io/badge/License-GPL%20v3-green.svg)](https://www.gnu.org/licenses/gpl-3.0)
[![Emacs Version](https://img.shields.io/badge/Emacs-26.1+-7F5AB6.svg?logo=gnu%20emacs&logoColor=white)](https://www.gnu.org/software/emacs/download.html)
[![Release](https://img.shields.io/github/release/emacs-eask/eask.svg?logo=github)](https://github.com/emacs-eask/eask/releases/latest)
[![npm](https://img.shields.io/npm/v/@emacs-eask/eask?logo=npm&color=green)](https://www.npmjs.com/package/@emacs-eask/eask)
[![npm-dm](https://img.shields.io/npm/dm/@emacs-eask/eask.svg)](https://npmcharts.com/compare/@emacs-eask/eask?minimal=true)

# eask
> A set of command-line tools to build Emacs packages

[![Commands](https://github.com/emacs-eask/eask/actions/workflows/test-commands.yml/badge.svg)](https://github.com/emacs-eask/eask/actions/workflows/test-commands.yml)
[![Compile](https://github.com/emacs-eask/eask/actions/workflows/test-redefine.yml/badge.svg)](https://github.com/emacs-eask/eask/actions/workflows/test-redefine.yml)

Eask is heavily inspired by Cask, so they are somewhat related! This tool focuses
on consistency! [Cask]() and [makem.sh]() both rely on bash which Windows doesn't
run on by default. If you use WSL or other environment system file Cygwin/MSYS
; then this may not be the tool you are looking for! 👀

##### Why Node.JS?

Node has a better support on all kind of terminal applications!

<!-- markdown-toc start - Don't edit this section. Run M-x markdown-toc-refresh-toc -->
**Table of Contents**

- [eask](#eask)
    - [📰 News](#📰-news)
    - [💾 Installation](#💾-installation)
    - [About CI/CD](#about-cicd)
    - [📝 About Eask file](#📝-about-eask-file)
    - [📂 Related Projects](#📂-related-projects)
    - [State of the project](#state-of-the-project)
    - [🏆 Goals for the project](#🏆-goals-for-the-project)
    - [Project Structure](#project-structure)
        - [📌 Dependencies](#📌-dependencies)
    - [📝 Todo list](#📝-todo-list)
    - [Contribute](#contribute)

<!-- markdown-toc end -->

## 📰 News

* `0.2.0` - Done basic error handling with exit code at the end of executions
* `0.1.39` - Use `spawn` instead `exec`; now messages will be printed immediately
* `0.1.0` - Project barebones are pretty much complete!

## 💾 Installation

The easiest way to install is through tool [npm](https://www.npmjs.com/),

```sh
$ npm install -g @emacs-eask/eask
```

Alternatively, you can clone it directly from this repo

```sh
# clone the repo
$ git clone https://github.com/emacs-eask/eask

# change the working directory to eask
$ cd eask

# install the requirements
$ npm install
```

Make sure you set up the environment path variable according to your system,

On Linux/macOS,

```sh
PATH=$PATH:/path/to/eask/bin
```

On Windows,

```batch
set PATH=%PATH%;c:/path/to/eask/bin
```

Once you have set it up correctly, try `eask --version` then you should see 
the current eask's version number! 🎉 🎊

## About CI/CD

```yml
jobs:
  test:
    runs-on: ${{ matrix.os }}
    strategy:
      matrix:
        os: [ubuntu-latest, macos-latest, windows-latest]
        emacs-version: [27.2, snapshot]

    steps:
    - uses: actions/checkout@v2

    # Install Emacs on Linux/macOS
    - uses: purcell/setup-emacs@master
      if: matrix.os == 'ubuntu-latest' || matrix.os == 'macos-latest'
      with:
        version: ${{ matrix.emacs-version }}

    # Install Emacs on Windows
    - uses: jcs090218/setup-emacs-windows@master
      if: matrix.os == 'windows-latest'
      with:
        version: ${{ matrix.emacs-version }}

    # You need node for eask
    - uses: actions/setup-node@v2
      with:
        node-version: '14'

    # Install eask
    - uses: emacs-eask/setup-eask@master
      with:
        version: 'snapshot'

    - name: Run tests
      run: |
        eask install
        eask compile
        eask lint
```

## 📝 About Eask file

`Eask` is the magic file that `eask` will read it as the init file in Emacs.
The syntaxes are very similar to the `Cask` file, but different.

```el
(source "gnu")

(package "your-package" "1.0.0" "Your package description")

(package-file "your-package-file.el")
```

Remember, `Eask` is just the regular elisp file, and should be read it from
the Emacs itself!

```el
; Regular Eask file content...

(setq byte-compile-error-on-warn t)  ; Singal error if warning occurred
```

`eask` provides some hooks so you can define your own action before/after
each command. The name of the hook follows the rule of
`eask-{before/after}-command-{%name%}-hook`.

For example, to enable compile on warn on `byte-compile` command

```el
(add-hook 'eask-before-command-compile-hook 
          (lambda () (setq byte-compile-error-on-warn t)))
```

## 📂 Related Projects

* [cask](https://github.com/cask/cask) - Project management tool for Emacs
* [makem.sh](https://github.com/alphapapa/makem.sh) - Makefile-like script for building and testing Emacs Lisp packages

## State of the project

The project barebones are pretty much done, we are currently looking for
contirbutors to give us feedback and improve our TUI/UX for this tool!

We are also looking for advices to add more Emacser often use commands and
options, so these features are prepared by default! Like command `lint` 
(package-lint) or option `--debug` refers to `debug-on-error` to `t`!

## 🏆 Goals for the project

1. Consistent sandbox testing across all systems
2. Has most commands that Emacsers often used (byte-compile, checkdoc, etc)
3. No dependencies installed by default; only install packages when it's needed!
4. Extra: Package Management Tool for your personal configuration/package

## Project Structure

The project structure are very simple, all we need is to look into 3 places.

1. `eask` file at the root of the project
2. `cmds` folder with all available commands
3. `lisp` folder with all elisp code

`eask` is the node entry, and the main yargs definition! `cmds` and `lisp`
folders are command files that correspond to each other.

The sandbox logic is stored inside file `./lisp/_prepare.el`.

### 📌 Dependencies

* [Node.js](https://nodejs.org/en/) and [npm](https://www.npmjs.com/)
* [yargs](https://github.com/yargs/yargs) to parse commands and options.
* [Emacs](https://www.gnu.org/software/emacs/) of course; make sure this is inside your environment PATH!

## 📝 Todo list

- [ ] logging with color
- [ ] [CI] add error test
- [ ] `checkdoc` command

## Contribute

N/A

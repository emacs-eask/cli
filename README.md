[![License: GPL v3](https://img.shields.io/badge/License-GPL%20v3-green.svg)](https://www.gnu.org/licenses/gpl-3.0)
[![Emacs Version](https://img.shields.io/badge/Emacs-26.1+-7F5AB6.svg?logo=gnu%20emacs&logoColor=white)](https://www.gnu.org/software/emacs/download.html)
[![Release](https://img.shields.io/github/release/emacs-eask/eask.svg?logo=github)](https://github.com/emacs-eask/eask/releases/latest)
[![npm](https://img.shields.io/npm/v/@emacs-eask/eask?logo=npm&color=green)](https://www.npmjs.com/package/@emacs-eask/eask)
[![npm-dm](https://img.shields.io/npm/dm/@emacs-eask/eask.svg)](https://npmcharts.com/compare/@emacs-eask/eask?minimal=true)

# eask
> A set of command-line tools to build Emacs packages

[![CI](https://github.com/emacs-eask/eask/actions/workflows/test.yml/badge.svg)](https://github.com/emacs-eask/eask/actions/workflows/test.yml)

Eask is heavily inspired by Cask, so they are somewhat related! This tool focuses
on Emacs built-in feature.

<!-- markdown-toc start - Don't edit this section. Run M-x markdown-toc-refresh-toc -->
**Table of Contents**

- [eask](#eask)
    - [📌 Dependencies](#📌-dependencies)
    - [💾 Installation](#💾-installation)
    - [About CI/CD](#about-cicd)
    - [📝 About Eask file](#📝-about-eask-file)
    - [Project Development](#project-development)
    - [Focus](#focus)
    - [Contribute](#contribute)

<!-- markdown-toc end -->

## 📌 Dependencies

* [Node.js](https://nodejs.org/en/)
* [yargs](https://github.com/yargs/yargs)
* [Emacs](https://www.gnu.org/software/emacs/) of course; make sure this is inside your environment PATH!

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

`Eask` is the magic file that `eask` will read it as init file in Emacs. The
syntax are very similar to `Cask` file, but different.

```el
(source "gnu")

(package "your-package" "1.0.0" "Your package description")

(package-file "your-package-file.el")
```

Remember, `Eask` is just the regular elisp file, and should be read it from
Emacs itself!

```el
; Regular Eask file content...

(setq byte-compile-error-on-warn t)  ; Singal error if warning occurred
```

`eask` provide some hooks so you can define your own action before/after 
each commands. The name of the hook follow the rule of 
`eask-{before/after}-command-{%name%}-hook`.

For example, to enable compile on warn on `byte-compile` command

```el
(add-hook 'eask-before-command-compile-hook 
          (lambda () (setq byte-compile-error-on-warn t)))
```

## 📂 Related Projects

* [cask](https://github.com/cask/cask) - Project management tool for Emacs
* [makem.sh](https://github.com/alphapapa/makem.sh) - Makefile-like script for building and testing Emacs Lisp packages

## Project Development

N/A

## Focus

N/A

## Contribute

N/A

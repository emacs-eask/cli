[![License: GPL v3](https://img.shields.io/badge/License-GPL%20v3-green.svg)](https://www.gnu.org/licenses/gpl-3.0)
[![Emacs Version](https://img.shields.io/badge/Emacs-26.1+-7F5AB6.svg?logo=gnu%20emacs&logoColor=white)](https://www.gnu.org/software/emacs/download.html)
[![Release](https://img.shields.io/github/release/emacs-eask/eask.svg?logo=github)](https://github.com/emacs-eask/eask/releases/latest)
[![npm](https://img.shields.io/npm/v/@emacs-eask/eask?logo=npm&color=green)](https://www.npmjs.com/package/@emacs-eask/eask)
[![npm-dm](https://img.shields.io/npm/dm/@emacs-eask/eask.svg)](https://npmcharts.com/compare/@emacs-eask/eask?minimal=true)

# eask
Emacs package command-line tool

[![CI](https://github.com/emacs-eask/eask/actions/workflows/test.yml/badge.svg)](https://github.com/emacs-eask/eask/actions/workflows/test.yml)

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

Easiest way to install, is through tool [npm](https://www.npmjs.com/),

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

## Project Development

N/A

## Focus

N/A

## Contribute

N/A

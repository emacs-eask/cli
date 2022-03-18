[![License: GPL v3](https://img.shields.io/badge/License-GPL%20v3-green.svg)](https://www.gnu.org/licenses/gpl-3.0)
[![Release](https://img.shields.io/github/release/emacs-eask/eask.svg?logo=github)](https://github.com/emacs-eask/eask/releases/latest)
[![npm](https://img.shields.io/npm/v/@emacs-eask/eask?logo=npm&color=green)](https://www.npmjs.com/package/@emacs-eask/eask)
[![npm-dm](https://img.shields.io/npm/dm/@emacs-eask/eask.svg)](https://npmcharts.com/compare/@emacs-eask/eask?minimal=true)

# eask
Emacs package command-line tool

[![CI](https://github.com/emacs-eask/eask/actions/workflows/test.yml/badge.svg)](https://github.com/emacs-eask/eask/actions/workflows/test.yml)

## Dependencies

* [Node.js](https://nodejs.org/en/)
* [yargs](https://github.com/yargs/yargs)
* [Emacs](https://www.gnu.org/software/emacs/) of course!

## 💾 Installation

Easiest way to install, is through tool [npm](https://www.npmjs.com/),

```sh
$ npm install -g @emacs-eask/eask
```

Alternatively, you can clone it directly from this repo

```sh
$ git clone https://github.com/emacs-eask/eask
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

N/A

## About Eask file

`Eask` is the magic file that `eask` will read it as init file in Emacs. The
syntax are very similar to `Cask` file, but different.

```el
(source "gnu")

(package "your-package" "1.0.0" "Your package description")

(package-file "your-package-name.el")
```

Remember, `Eask` are just regular elisp file, and can be read it from Emacs itself!

```el
; Regular Eask file content...

(setq byte-compile-error-on-warn t)  ; Singal error if warning occurred
```

## Project Development

N/A

## Contribute

N/A

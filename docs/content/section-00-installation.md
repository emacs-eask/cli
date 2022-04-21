---
title: Installation
permalink: installation
---

# Installation

This document guides you through the installation of Eask.

## 🚩 Prerequisites

Eask requires GNU Emacs 26 and Node 14 or later! It will not work with Emacs 25
and below, or with other flavours of Emacs, e.g. XEmacs.

1. [Node.js](https://nodejs.org/en/) and [npm](https://www.npmjs.com/) for being the CLI layer
2. [yargs](https://github.com/yargs/yargs) to parse commands and options.
3. [Emacs](https://www.gnu.org/software/emacs/) of course; make sure this is inside your environment PATH!

## 💾 Install through NPM

The easiest way to install is through tool [npm](https://www.npmjs.com/),

```sh
$ npm install -g @emacs-eask/eask
```

## 💾 Manual installation

Alternatively, you can clone it directly from this repo

```sh
# clone the repo
$ git clone https://github.com/emacs-eask/eask

# change the working directory to eask
$ cd eask

# install the requirements
$ npm install
```

## 🏡 Setup

Make sure you set up the environment path variable according to your system,

On Linux/macOS,

```sh
export PATH="$HOME/eask/bin:$PATH"
```

On Windows,

```batch
set PATH=%PATH%;c:/path/to/eask/bin
```

Once you have set it up correctly, try `eask --version` then you should see 
the current eask's version number! 🎉 🎊

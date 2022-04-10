---
title: Contributing
permalink: contributing
---

# Contributing

This document provides guidelines and information on contributing to Eask.

## ⚜️ Code of Conduct

Contributors to Eask should abide by the [Contributor Covenant](https://www.contributor-covenant.org/version/1/4/code-of-conduct).

## 📂 State of the project

The project's barebones are pretty much done, we are currently looking for
contributors to give us feedback and improve our TUI/UX for this tool!

We are also looking for advice to add more Emacser often use commands and
options, so these features are prepared by default! Like command `lint` 
(package-lint) or option `--debug` refers to `debug-on-error` to `t`!

## Developing Eask

### 🚩 Prerequisites

To make changes to Eask, you should have:

1. [Node.js](https://nodejs.org/en/) the development environment.
2. [npm](https://www.npmjs.com/) for the package manager.
3. [yargs](https://github.com/yargs/yargs) for parsing the command-line.
4. [Emacs](https://www.gnu.org/software/emacs/), 26.1 or above!

### Building

To build the development environment, you would have to install Eask using
the [manual installation](https://emacs-eask.github.io/eask/installation#-manual-installation)
method. (Not install through NPM) Make sure you have setup the environment
PATH variable, so you can call `eask` from the terminal.

After you have step throught the installation, try:

```sh
eask locate
```

It should print out the location of your working Eask workspace. You should
able to identify the Eask workspace, even you have multiple Eask version
installed!

### Testing

Eask does not offer local testing, all our tests are accomplished using GitHub
Actions. Please fork our repository, and push your changes to your fork. GitHub
Actions should pick up the test for you!

Make sure you have GitHub Actions enabled in your repository (forked). Got to
**Settings** -> **Actions** -> **General** -> **Actions Permissions**; make sure
you have checked the correct options.

## Documentation

Eask includes a comprehensive user guide. Please try to extend it accordingly while
you implement new features.

The documentation is written in markdown using GitHub Pages service. You can find all
our documentation under the `docs` folder from the project root.

## Pull requests

If all tests passes, and Eask can operates normally with updated documentation
(if any), please send us a [pull request](https://github.com/emacs-eask/eask/pulls)
with your changes. 🎊

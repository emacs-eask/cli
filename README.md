# eask
> Command-line tool for building and testing Emacs Lisp packages

[![License: GPL v3](https://img.shields.io/badge/License-GPL%20v3-green.svg)](https://www.gnu.org/licenses/gpl-3.0)
[![Emacs Version](https://img.shields.io/badge/Emacs-26.1+-7F5AB6.svg?logo=gnu%20emacs&logoColor=white)](https://www.gnu.org/software/emacs/download.html)
[![Release](https://img.shields.io/github/release/emacs-eask/eask.svg?logo=github)](https://github.com/emacs-eask/eask/releases/latest)
[![npm](https://img.shields.io/npm/v/@emacs-eask/eask?logo=npm&color=green)](https://www.npmjs.com/package/@emacs-eask/eask)
[![npm-dm](https://img.shields.io/npm/dm/@emacs-eask/eask.svg)](https://npmcharts.com/compare/@emacs-eask/eask?minimal=true)

Eask is a command-line tool that helps you build, lint, and test Emacs Lisp
packages. It creates a clean environment to sandbox your elisp code without
influencing your personal confiugration. Eask aims to be:

* **Consistent** enough to sandbox across all systems
* **General** enough to have Emacsers often used commands (byte-compile, checkdoc, etc)
* **Robust** enough to provide useful results even in the presense of user's errors
* **Dependency-free** so that the tool can be run on any platform

## 🔗 Links

* [Documentation](https://emacs-eask.github.io/)
* [Command-line interface](https://emacs-eask.github.io/Getting-Started/Commands-and-options/)
* [Examples](https://emacs-eask.github.io/Examples/Real-project-examples/)

## 🧪 Testing

##### ✔️ Tests puppose to be `GREEN`

###### Documentation

[![Docs](https://github.com/emacs-eask/eask/actions/workflows/docs.yml/badge.svg)](https://github.com/emacs-eask/eask/actions/workflows/docs.yml)

###### Development

[![Compile](https://github.com/emacs-eask/eask/actions/workflows/compile.yml/badge.svg)](https://github.com/emacs-eask/eask/actions/workflows/compile.yml)
[![Compat](https://github.com/emacs-eask/eask/actions/workflows/compat.yml/badge.svg)](https://github.com/emacs-eask/eask/actions/workflows/compat.yml)
[![Build](https://github.com/emacs-eask/eask/actions/workflows/build.yml/badge.svg)](https://github.com/emacs-eask/eask/actions/workflows/build.yml)

###### Commands

[![Global](https://github.com/emacs-eask/eask/actions/workflows/global.yml/badge.svg)](https://github.com/emacs-eask/eask/actions/workflows/global.yml)
[![Local](https://github.com/emacs-eask/eask/actions/workflows/local.yml/badge.svg)](https://github.com/emacs-eask/eask/actions/workflows/local.yml)
[![Install](https://github.com/emacs-eask/eask/actions/workflows/install.yml/badge.svg)](https://github.com/emacs-eask/eask/actions/workflows/install.yml)
[![Exec](https://github.com/emacs-eask/eask/actions/workflows/exec.yml/badge.svg)](https://github.com/emacs-eask/eask/actions/workflows/exec.yml)
[![Search](https://github.com/emacs-eask/eask/actions/workflows/search.yml/badge.svg)](https://github.com/emacs-eask/eask/actions/workflows/search.yml)
[![Outdated_Upgrade](https://github.com/emacs-eask/eask/actions/workflows/outdated_upgrade.yml/badge.svg)](https://github.com/emacs-eask/eask/actions/workflows/outdated_upgrade.yml)
[![Checker](https://github.com/emacs-eask/eask/actions/workflows/checker.yml/badge.svg)](https://github.com/emacs-eask/eask/actions/workflows/checker.yml)

###### Features

[![Color](https://github.com/emacs-eask/eask/actions/workflows/color.yml/badge.svg)](https://github.com/emacs-eask/eask/actions/workflows/color.yml)

###### Test

[![Test ert](https://github.com/emacs-eask/eask/actions/workflows/test_ert.yml/badge.svg)](https://github.com/emacs-eask/eask/actions/workflows/test_ert.yml)
[![Test ert-runner](https://github.com/emacs-eask/eask/actions/workflows/test_ert-runner.yml/badge.svg)](https://github.com/emacs-eask/eask/actions/workflows/test_ert-runner.yml)
[![Test buttercup](https://github.com/emacs-eask/eask/actions/workflows/test_buttercup.yml/badge.svg)](https://github.com/emacs-eask/eask/actions/workflows/test_buttercup.yml)

##### ❌ Tests puppose to be `RED`

[![Error](https://github.com/emacs-eask/eask/actions/workflows/error.yml/badge.svg)](https://github.com/emacs-eask/eask/actions/workflows/error.yml)

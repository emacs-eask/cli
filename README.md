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
* [Command-line interface](https://emacs-eask.github.io/eask/usage)

## 🧪 Testing

##### ✔️ Tests puppose to be `GREEN`

###### Documentation

[![pages-build-deployment](https://github.com/emacs-eask/eask/actions/workflows/pages/pages-build-deployment/badge.svg)](https://github.com/emacs-eask/eask/actions/workflows/pages/pages-build-deployment)

###### Development

[![Compile](https://github.com/emacs-eask/eask/actions/workflows/test-redefine.yml/badge.svg)](https://github.com/emacs-eask/eask/actions/workflows/test-redefine.yml)

###### Commands

[![Global](https://github.com/emacs-eask/eask/actions/workflows/test-global.yml/badge.svg)](https://github.com/emacs-eask/eask/actions/workflows/test-global.yml)
[![Local](https://github.com/emacs-eask/eask/actions/workflows/test-local.yml/badge.svg)](https://github.com/emacs-eask/eask/actions/workflows/test-local.yml)
[![Install](https://github.com/emacs-eask/eask/actions/workflows/test-install.yml/badge.svg)](https://github.com/emacs-eask/eask/actions/workflows/test-install.yml)
[![Exec](https://github.com/emacs-eask/eask/actions/workflows/test-exec.yml/badge.svg)](https://github.com/emacs-eask/eask/actions/workflows/test-exec.yml)
[![Search](https://github.com/emacs-eask/eask/actions/workflows/test-search.yml/badge.svg)](https://github.com/emacs-eask/eask/actions/workflows/test-search.yml)
[![Outdated-Upgrade](https://github.com/emacs-eask/eask/actions/workflows/test-outdated-upgrade.yml/badge.svg)](https://github.com/emacs-eask/eask/actions/workflows/test-outdated-upgrade.yml)

###### Features

[![Color](https://github.com/emacs-eask/eask/actions/workflows/test-color.yml/badge.svg)](https://github.com/emacs-eask/eask/actions/workflows/test-color.yml)

##### ❌ Tests puppose to be `RED`

[![Error](https://github.com/emacs-eask/eask/actions/workflows/test-error.yml/badge.svg)](https://github.com/emacs-eask/eask/actions/workflows/test-error.yml)

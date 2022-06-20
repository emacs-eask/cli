<a href="#"><img align="right" src="./docs/static/logo.png" width="20%"></a>

# Eask
> Command-line tool for building and testing Emacs Lisp packages

[![License: GPL v3](https://img.shields.io/badge/License-GPL%20v3-green.svg)](https://www.gnu.org/licenses/gpl-3.0)
[![Emacs Version](https://img.shields.io/badge/Emacs-26.1+-7F5AB6.svg?logo=gnu%20emacs&logoColor=white)](https://www.gnu.org/software/emacs/download.html)
[![Release](https://img.shields.io/github/release/emacs-eask/eask.svg?logo=github)](https://github.com/emacs-eask/eask/releases/latest)

Eask is a command-line tool that helps you build, lint, and test Emacs Lisp
packages. It creates a clean environment to sandbox your elisp code without
influencing your personal configuration. Eask aims to be:

* **Consistent** enough to sandbox across all systems
* **General** enough to have Emacsers often used commands (byte-compile, checkdoc, etc)
* **Robust** enough to provide useful results even in the presence of user errors
* **Dependency-free** so that the tool can be run on any platform

## 🔗 Links

* [Documentation](https://emacs-eask.github.io/)
* [Installation](https://emacs-eask.github.io/Getting-Started/Install-Eask/)
* [Command-line interface](https://emacs-eask.github.io/Getting-Started/Commands-and-options/)
* [Examples](https://emacs-eask.github.io/Examples/Real-project-examples/)

## 🧪 Testing

###### Documentation

| Description                            | Done | Status                                                                                                                                            |
|----------------------------------------|------|---------------------------------------------------------------------------------------------------------------------------------------------------|
| Keep the documentation page up to date | ✔    | [![Docs](https://github.com/emacs-eask/eask/actions/workflows/docs.yml/badge.svg)](https://github.com/emacs-eask/eask/actions/workflows/docs.yml) |

###### Development

| Description                               | Done | Status                                                                                                                                                     |
|-------------------------------------------|------|------------------------------------------------------------------------------------------------------------------------------------------------------------|
| Compile source and check redefined        | ✔    | [![Compile](https://github.com/emacs-eask/eask/actions/workflows/compile.yml/badge.svg)](https://github.com/emacs-eask/eask/actions/workflows/compile.yml) |
| Compatibility check for each Emacs version | ✔    | [![Compat](https://github.com/emacs-eask/eask/actions/workflows/compat.yml/badge.svg)](https://github.com/emacs-eask/eask/actions/workflows/compat.yml)    |
| Build executables                         | ✔    | [![Build](https://github.com/emacs-eask/eask/actions/workflows/build.yml/badge.svg)](https://github.com/emacs-eask/eask/actions/workflows/build.yml)       |

###### Commands

| Description                                 | Done | Status                                                                                                                                                                                |
|---------------------------------------------|------|---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
| Test commands in global (config) mode       | ✔    | [![Global](https://github.com/emacs-eask/eask/actions/workflows/global.yml/badge.svg)](https://github.com/emacs-eask/eask/actions/workflows/global.yml)                               |
| Test commands in development (package) mode | ✔    | [![Local](https://github.com/emacs-eask/eask/actions/workflows/local.yml/badge.svg)](https://github.com/emacs-eask/eask/actions/workflows/local.yml)                                  |
| Test to install packages                    | ✔    | [![Install](https://github.com/emacs-eask/eask/actions/workflows/install.yml/badge.svg)](https://github.com/emacs-eask/eask/actions/workflows/install.yml)                            |
| Test execute commands                       | ✔    | [![Exec](https://github.com/emacs-eask/eask/actions/workflows/exec.yml/badge.svg)](https://github.com/emacs-eask/eask/actions/workflows/exec.yml)                                     |
| Test emacs commands                         | ✔    | [![Emacs](https://github.com/emacs-eask/eask/actions/workflows/emacs.yml/badge.svg)](https://github.com/emacs-eask/eask/actions/workflows/emacs.yml)                                  |
| Test to search packages                     | ✔    | [![Search](https://github.com/emacs-eask/eask/actions/workflows/search.yml/badge.svg)](https://github.com/emacs-eask/eask/actions/workflows/search.yml)                               |
| Test to upgrade and check outdated packages | ✔    | [![Outdated_Upgrade](https://github.com/emacs-eask/eask/actions/workflows/outdated_upgrade.yml/badge.svg)](https://github.com/emacs-eask/eask/actions/workflows/outdated_upgrade.yml) |
| `Eask`-file checker                         | ✔    | [![Checker](https://github.com/emacs-eask/eask/actions/workflows/checker.yml/badge.svg)](https://github.com/emacs-eask/eask/actions/workflows/checker.yml)                            |

###### Options

| Description          | Done | Status                                                                                                                                                     |
|----------------------|------|------------------------------------------------------------------------------------------------------------------------------------------------------------|
| Test option switches | ✔    | [![Options](https://github.com/emacs-eask/eask/actions/workflows/options.yml/badge.svg)](https://github.com/emacs-eask/eask/actions/workflows/options.yml) |

###### Test

| Description               | Done | Status                                                                                                                                                                             |
|---------------------------|------|------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
| Test `ert` command        | ✔    | [![Test ert](https://github.com/emacs-eask/eask/actions/workflows/test_ert.yml/badge.svg)](https://github.com/emacs-eask/eask/actions/workflows/test_ert.yml)                      |
| Test `ert-runner` command | ✔    | [![Test ert-runner](https://github.com/emacs-eask/eask/actions/workflows/test_ert-runner.yml/badge.svg)](https://github.com/emacs-eask/eask/actions/workflows/test_ert-runner.yml) |

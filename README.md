<a href="#"><img align="right" src="./docs/static/logo.png" width="20%"></a>
# Eask
> CLI for building, running, testing, and managing your Emacs Lisp dependencies

[![License: GPL v3](https://img.shields.io/badge/License-GPL%20v3-green.svg)](https://www.gnu.org/licenses/gpl-3.0)
[![Emacs Version](https://img.shields.io/badge/Emacs-26.1+-7F5AB6.svg?logo=gnu%20emacs&logoColor=white)](https://www.gnu.org/software/emacs/download.html)
[![Release](https://img.shields.io/github/release/emacs-eask/cli.svg?logo=github)](https://github.com/emacs-eask/cli/releases/latest)
[![Discord](https://img.shields.io/discord/1131434607213023262?label=Discord&logo=discord&logoColor=white&color=7289DA)](https://discord.gg/E9zzjWGfFD)

Eask was originally designed as a package development tool for Elisp projects.
However, it has since expanded to support a wide range of Emacs Lisp tasks.
It can now be used in three major ways:

1. As a development tool for Elisp packages.
2. For managing dependencies in your Emacs configuration.
3. To run Elisp programs for a variety of purposes (essentially functioning as a runtime).

With these capabilities in mind, what sets Eask apart from other build tools
like [Cask][], [makem.sh][], and [Eldev][]?

Great question! Eask has evolved beyond just a build tool—it serves multiple purposes! Here’s what Eask aims to be:

- **Consistent**: Provides a reliable sandboxing environment across all systems.
- **Versatile**: Includes commonly used Emacs commands like `byte-compilation`, `checkdoc`, and more.
- **Robust**: Delivers useful results even when user errors occur.
- **Lightweight**: Runs on any platform without dependencies.

*📝 P.S. See [Why Eask?](https://emacs-eask.github.io/Getting-Started/Introduction/#-why-eask) for more detailed
information.*

## 🔗 Links
> 💡 [`node`][node] is not required to use Eask!

- [Documentation](https://emacs-eask.github.io/)
- [Installation](https://emacs-eask.github.io/Getting-Started/Install-Eask/)
- [Command-line interface](https://emacs-eask.github.io/Getting-Started/Commands-and-options/)
- [Examples](https://emacs-eask.github.io/Examples/Real-project-examples/)
- [FAQ](https://emacs-eask.github.io/FAQ/)

## 🧪 Testing

We have incorporated a range of tests to ensure Eask remains stable throughout its release cycle.

###### Documentation

| Description                            | Done | Status                                                                                                                                          |
|----------------------------------------|------|-------------------------------------------------------------------------------------------------------------------------------------------------|
| Keep the documentation page up to date | ✔    | [![Docs](https://github.com/emacs-eask/cli/actions/workflows/docs.yml/badge.svg)](https://github.com/emacs-eask/cli/actions/workflows/docs.yml) |

###### Development

| Description                                | Done | Status                                                                                                                                                   |
|--------------------------------------------|------|----------------------------------------------------------------------------------------------------------------------------------------------------------|
| Compile source and check redefined         | ✔    | [![Compile](https://github.com/emacs-eask/cli/actions/workflows/compile.yml/badge.svg)](https://github.com/emacs-eask/cli/actions/workflows/compile.yml) |
| Compatibility check for each Emacs version | ✔    | [![Compat](https://github.com/emacs-eask/cli/actions/workflows/compat.yml/badge.svg)](https://github.com/emacs-eask/cli/actions/workflows/compat.yml)    |
| Build executables                          | ✔    | [![Build](https://github.com/emacs-eask/cli/actions/workflows/build.yml/badge.svg)](https://github.com/emacs-eask/cli/actions/workflows/build.yml)       |

###### Commands

| Description                                  | Done | Status                                                                                                                                                                              |
|----------------------------------------------|------|-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
| Test commands in global (`~/.eask/`) mode    | ✔    | [![Global](https://github.com/emacs-eask/cli/actions/workflows/global.yml/badge.svg)](https://github.com/emacs-eask/cli/actions/workflows/global.yml)                               |
| Test commands in config (`~/.emacs.d/`) mode | ✔    | [![Confg](https://github.com/emacs-eask/cli/actions/workflows/config.yml/badge.svg)](https://github.com/emacs-eask/cli/actions/workflows/config.yml)                                |
| Test commands in development (`./`) mode     | ✔    | [![Local](https://github.com/emacs-eask/cli/actions/workflows/local.yml/badge.svg)](https://github.com/emacs-eask/cli/actions/workflows/local.yml)                                  |
| Test install packages                        | ✔    | [![Install](https://github.com/emacs-eask/cli/actions/workflows/install.yml/badge.svg)](https://github.com/emacs-eask/cli/actions/workflows/install.yml)                            |
| Test link packages                           | ✔    | [![Link](https://github.com/emacs-eask/cli/actions/workflows/link.yml/badge.svg)](https://github.com/emacs-eask/cli/actions/workflows/link.yml)                                     |
| Test `analyze` command / `Eask`-file checker | ✔    | [![Analyze](https://github.com/emacs-eask/cli/actions/workflows/analyze.yml/badge.svg)](https://github.com/emacs-eask/cli/actions/workflows/analyze.yml)                            |
| Test `docker` command                        | ✔    | [![Docker](https://github.com/emacs-eask/cli/actions/workflows/docker.yml/badge.svg)](https://github.com/emacs-eask/cli/actions/workflows/docker.yml)                               |
| Test `exec` command                          | ✔    | [![Exec](https://github.com/emacs-eask/cli/actions/workflows/exec.yml/badge.svg)](https://github.com/emacs-eask/cli/actions/workflows/exec.yml)                                     |
| Test `emacs` command                         | ✔    | [![Emacs](https://github.com/emacs-eask/cli/actions/workflows/emacs.yml/badge.svg)](https://github.com/emacs-eask/cli/actions/workflows/emacs.yml)                                  |
| Test search packages                         | ✔    | [![Search](https://github.com/emacs-eask/cli/actions/workflows/search.yml/badge.svg)](https://github.com/emacs-eask/cli/actions/workflows/search.yml)                               |
| Test upgrade and check outdated packages     | ✔    | [![Outdated_Upgrade](https://github.com/emacs-eask/cli/actions/workflows/outdated_upgrade.yml/badge.svg)](https://github.com/emacs-eask/cli/actions/workflows/outdated_upgrade.yml) |
| Test `upgrade-eask` command                  | ✔    | [![Upgrade Eask](https://github.com/emacs-eask/cli/actions/workflows/upgrade-eask.yml/badge.svg)](https://github.com/emacs-eask/cli/actions/workflows/upgrade-eask.yml)             |

###### Options

| Description          | Done | Status                                                                                                                                                   |
|----------------------|------|----------------------------------------------------------------------------------------------------------------------------------------------------------|
| Test option switches | ✔    | [![Options](https://github.com/emacs-eask/cli/actions/workflows/options.yml/badge.svg)](https://github.com/emacs-eask/cli/actions/workflows/options.yml) |

###### Test

| Description               | Done | Status                                                                                                                                                                           |
|---------------------------|------|----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
| Test `ert` command        | ✔    | [![Test ert](https://github.com/emacs-eask/cli/actions/workflows/test_ert.yml/badge.svg)](https://github.com/emacs-eask/cli/actions/workflows/test_ert.yml)                      |
| Test `ert-runner` command | ✔    | [![Test ert-runner](https://github.com/emacs-eask/cli/actions/workflows/test_ert-runner.yml/badge.svg)](https://github.com/emacs-eask/cli/actions/workflows/test_ert-runner.yml) |
| Test `buttercup` command  | ✔    | [![Test buttercup](https://github.com/emacs-eask/cli/actions/workflows/test_buttercup.yml/badge.svg)](https://github.com/emacs-eask/cli/actions/workflows/test_buttercup.yml)    |
| Test `ecukes` command     | ✔    | [![Test ecukes](https://github.com/emacs-eask/cli/actions/workflows/test_ecukes.yml/badge.svg)](https://github.com/emacs-eask/cli/actions/workflows/test_ecukes.yml)             |

###### Others

| Description | Done | Status                                                                                                                                                               |
|-------------|------|----------------------------------------------------------------------------------------------------------------------------------------------------------------------|
| Webinstall  | ✔    | [![Webinstall](https://github.com/emacs-eask/cli/actions/workflows/webinstall.yml/badge.svg)](https://github.com/emacs-eask/cli/actions/workflows/webinstall.yml)    |
| Exit Status | ✔    | [![Exit Status](https://github.com/emacs-eask/cli/actions/workflows/exit_status.yml/badge.svg)](https://github.com/emacs-eask/cli/actions/workflows/exit_status.yml) |

## ⚜️ License

This program is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program.  If not, see <https://www.gnu.org/licenses/>.

See [`LICENSE`](./LICENSE) for details.


<!-- Links -->

[Cask]: https://github.com/cask/cask
[makem.sh]: https://github.com/alphapapa/makem.sh
[Eldev]: https://github.com/doublep/eldev

[node]: https://nodejs.org/

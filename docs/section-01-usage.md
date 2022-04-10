---
title: Usage
permalink: usage
---

# Usage

This document explains how to use Eask, and provides a reference of its commands and options.

## Quickstart

Start by creating a file named `Eask` in the project root. Use **eask init*
command to create a Eask-file. You will be asked to enter few questions
to create the file.

```sh
$ eask init
```

To install all dependencies, run:

```sh
$ eask install-deps
```

💡 Use [-g] options for your Emacs configuration. Otherwise, it will create
a directory named `.eask` and install all dependencies into it.

### Finding Emacs

By default, Eask will use whatever the Emacs version exists in your environment
path. Use **emacs --version** to check your Emacs version.

There is currently no way to specify an Emacs version to execute.

## Commands

The gneeral syntax of the **eask** program is:

```sh
$ eask [GLOBAL-OPTIONS] [COMMAND] [COMMAND-OPTIONS] [COMMAND-ARGUMENTS]
```

#### eask init

Eask will generate file like:

```elisp
(package "PACKAGE-NAME"
         "VERSION"
         "YOUR PACKAGE SUMMARY")

(package-file "PACKAGE-FILE")

(source "gnu")
```

💡 See section [Examples](https://emacs-eask.github.io/eask/examples) for more information!

#### eask info

Show information about the project or configuration.

```sh
$ eask [GLOBAL-OPTIONS] info
```

#### eask install-deps

To install all dependencies.

```sh
$ eask [GLOBAL-OPTIONS] install-deps [--dev]
```

💡 Specify option [--dev] to install dependencies from the development scope.

#### eask install

To install packages.

```sh
$ eask [GLOBAL-OPTIONS] install [PACKAGES..]
```

Install packages by specifying arguments:

```sh
$ eask install auto-complete helm magit
```

Or else, it will install the package from the current development:

```sh
$ eask install
```

#### eask uninstall

To uninstall/delete packages.

```sh
$ eask [GLOBAL-OPTIONS] uninstall [PACKAGES..]
```

Uninstall packages by specifying arguments:

```sh
$ eask uninstall dash f s
```

Or else, it will uninstall the package from the current development:

```sh
$ eask uninstall
```

#### eask package

WIP

#### eask outdated

### Clean up

#### eask clean

Delete `.eask` from current workspace.

⛔️ Don't specify option `--global, -g`, or else it will delete your entire
`~/.emacs.d`

```elisp
$ eask clean -g
```

#### eask clean-elc

Delete all `.elc` files. This would respect to your `Eask` file.

#### eask clean-all

This is equivalent to `eask clean` + `eask clean-elc`.

### Linting

#### eask lint [FILES..]

WIP

#### eask checkdoc [FILES..]

WIP

## Options

#### --global, -g

Use `~/.emacs.d/` instead of package development environment. This is used
for Emacs configuration.

Install package `auto-complete` for your Emacs configuration:

```sh
$ eask install auto-complete -g
```

#### --development, --dev

Notify command with development scope enabled.

If we attempt to install development dependencies:

```sh
$ eask install --dev
```

#### --force, -f

Force command's execution.

Force to uninstall package `dash` even it's dependencies from other packages

```sh
$ eask uninstall dash -f
```

#### --debug

Enable `debug-on-error`.

This is equivalent to:

```elisp
(setq debug-on-error t)
```

#### --strict

WIP

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

## Commands and options

The gneeral syntax of the **eask** program is:

```sh
$ eask [GLOBAL-OPTIONS] [COMMAND] [COMMAND-OPTIONS] [COMMAND-ARGUMENTS]
```

#### 🔎 eask init

Eask will generate file like:

```elisp
(package "PACKAGE-NAME"
         "VERSION"
         "YOUR PACKAGE SUMMARY")

(package-file "PACKAGE-FILE")

(source "gnu")

(depends-on "emacs" "26.1")
```

💡 See section [Examples](https://emacs-eask.github.io/eask/examples) for more information!

#### 🔎 eask info

Show information about the project or configuration.

```sh
$ eask [GLOBAL-OPTIONS] info
```

#### 🔎 eask install-deps

To install all dependencies.

```sh
$ eask [GLOBAL-OPTIONS] install-deps [--dev]
```

💡 Specify option [--dev] to install dependencies from the development scope.

#### 🔎 eask install

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

#### 🔎 eask uninstall

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

#### 🔎 eask package

Build the package artifact.

```sh
$ eask package [DESTINATION]
```

If [DESTINATION] is not specified, it will export to the `/dist` folder
by default.

#### 🔎 eask compile

Byte-compile files.

```sh
$ eask compile [FILES..]
```

Compile files by specifying arguments:

```sh
$ eask compile file-1.el file-2.el
```

Or compile files that are already specified in your `Eask`-file.

```sh
$ eask compile
```

#### 🔎 eask autoloads

Generate autoload file.

```sh
$ eask [GLOBAL-OPTIONS] autoloads
```

#### 🔎 eask pkg-file

Write a package descriptor file to `project-pkg.el` in the project root.

```sh
$ eask [GLOBAL-OPTIONS] pkg-file
```

`project` is the project name, as declared in `Eask`-file. See
[Multi-file Packages (elisp)](https://www.gnu.org/software/emacs/manual/html_node/elisp/Multi_002dfile-Packages.html#Multi_002dfile-Packages)
for details.

#### 🔎 eask package-directory

Print path to package directory, where all dependencies are installed.

```sh
$ eask [GLOBAL-OPTIONS] package-directory
```

#### 🔎 eask paths

Print the `PATH` environment variable of this project.

```sh
$ eask [GLOBAL-OPTIONS] path
```

#### 🔎 eask load-path

Print the load path containing the dependencies of the current project.

```sh
$ eask [GLOBAL-OPTIONS] load-path
```

#### 🔎 eask files

Print the list of all package files.

```sh
$ eask [GLOBAL-OPTIONS] files
```

#### 🔎 eask exec

Execute the system command with the given arguments.

```sh
$ eask [GLOBAL-OPTIONS] exec [COMMAND] [ARGUMENTS ...]
```

This is only compatible with Cask's exec command. Eask does not play with
your environment $PATH; all execution only happens inside Emacs.

#### 🔎 eask eval

Evaluate `FORM` as a lisp form.

```sh
$ eask [GLOBAL-OPTIONS] eval [FORM]
```

#### 🔎 eask load

Load Emacs Lisp files in order.

```sh
$ eask [GLOBAL-OPTIONS] load [FILES..]
```

#### 🔎 eask concate

Concatenate all Emacs Lisp files into one big file.

```sh
$ eask [GLOBAL-OPTIONS] concate [FILES..]
```

### Management

#### 🔎 eask archives

List out all package archives.

```sh
$ eask [GLOBAL-OPTIONS] archives
```

#### 🔎 eask search

Search packages from archives.

```sh
$ eask [GLOBAL-OPTIONS] search [QUEIRES..]
```

#### 🔎 eask upgrade

Upgrade all packages.

```sh
$ eask [GLOBAL-OPTIONS] upgrade
```

#### 🔎 eask list

List out all installed packages.

```sh
$ eask [GLOBAL-OPTIONS] list [--depth]
```

#### 🔎 eask list-all

List out all available packages.

```sh
$ eask [GLOBAL-OPTIONS] list-all [--depth]
```

#### 🔎 eask outdated

List out all outdated packages.

```sh
$ eask [GLOBAL-OPTIONS] outdated [--depth]
```

#### 🔎 eask refresh

Download package archives.

```sh
$ eask [GLOBAL-OPTIONS] refresh
```

### Cleanup

#### 🔎 eask clean

Delete `.eask` from the current workspace.

```sh
$ eask [GLOBAL-OPTIONS] clean
```

⛔️ Don't specify the option `--global, -g`, or else it will delete your entire
`~/.emacs.d`

```elisp
$ eask clean -g
```

#### 🔎 eask clean-elc

Delete all `.elc` files. This would respect to your `Eask` file.

```sh
$ eask [GLOBAL-OPTIONS] clean-elc
```

#### 🔎 eask clean-all

This command is combination of all other clean commands.

* `clean`
* `clean-elc`

```sh
$ eask [GLOBAL-OPTIONS] clean-all
```

### Lint

#### 🔎 eask lint [FILES..]

Lint package using [package-lint](https://github.com/purcell/package-lint).

```sh
$ eask [GLOBAL-OPTIONS] lint [FILES..]
```

#### 🔎 eask checkdoc [FILES..]

Run checkdoc.

```sh
$ eask [GLOBAL-OPTIONS] checkdoc [FILES..]
```

#### 🔎 eask ert [FILES..]

```sh
$ eask [GLOBAL-OPTIONS] ert [FILES..]
```

#### 🔎 eask ert-runner [FILES..]

```sh
$ eask [GLOBAL-OPTIONS] ert-runner [FILES..]
```

### Utilities

#### 🔎 eask upgrade-eask

Upgrade Eask to the latest version.

```sh
$ eask [GLOBAL-OPTIONS] upgrade-eask
```

#### 🔎 eask locate

Show Eask installed location

```sh
$ eask [GLOBAL-OPTIONS] locate
```

## Global Options

The following options are available on all Eask commands:

#### 🔎 --global, -g

Use `~/.emacs.d/` instead of package development environment. This is used
for Emacs configuration.

Install package `auto-complete` for your Emacs configuration:

```sh
$ eask install auto-complete -g
```

#### 🔎 --development, --dev

Notify command with development scope enabled.

If we attempt to install development dependencies:

```sh
$ eask install --dev
```

#### 🔎 --force, -f

Force command's execution.

Force to uninstall package `dash` even it's dependencies from other packages

```sh
$ eask uninstall dash -f
```

#### 🔎 --debug

Enable debug information.

This is equivalent to:

```elisp
(setq debug-on-error t)
```

#### 🔎 --strict

Trigger error instead of warnings.

For instance, in **eask compile**:

```elisp
(setq byte-compile-error-on-warn t)
```

#### 🔎 --allow-error

Continue the execution without killing the Emacs.

#### 🔎 --insecure

Connect archives with HTTP instead of HTTPS.

#### 🔎 --timestamps, --no-timestamps

Enable/Disable timestamps.

#### 🔎 --log-level, --no-log-level

Enable/Disable log header.

#### 🔎 --no-color

Disable color output.

#### 🔎 --proxy `<proxy>`

Set Emacs proxy for HTTP and HTTPS:

```sh
$ eask --proxy "localhost:8888" install
```

#### 🔎 --http-proxy `<proxy>`

Set Emacs proxy for HTTP only.

#### --https-proxy `<proxy>`

Set Emacs proxy for HTTPS only.

#### 🔎 --no-proxy `<pattern>`

Do not use a proxy for any URL matching pattern.

`<pattern>`is an Emacs regular expression.

#### 🔎 --verbose, -v `<integer>`

Set verbosity from 0 to 4.

```sh
--verbose 4
```

#### 🔎 --version

Show version number.

#### 🔎 --help

Show help.

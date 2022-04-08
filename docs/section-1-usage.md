---
title: Usage
permalink: usage
---

# Usage

This document explains how to use Eask, and provides a reference of its commands and options.

## Quickstart

Start by creating a file named `Eask` in the project root. Use eask init
command to create a Eask-file automatically, containing boilerplate code:

```sh
$ eask init
```

To install all dependencies:

```sh
$ eask install-deps
```

```sh
$ eask install
```

```sh
$ eask uninstall
```

```sh
$ eask package
```

```sh
$ eask outdated
```

### Clean up

```sh
$ eask clean
```

```sh
$ eask clean-elc
```

```sh
$ eask clean-all
```

![](./assets/screenshot/outdated.png)

### Linting

```sh
$ eask lint
```

```sh
$ eask checkdoc
```

### Options

```
--global, -g
```

```
--development, --dev
```

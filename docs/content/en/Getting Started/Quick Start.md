---
title: 🔰 Quick Start
weight: 100
---

{{< toc >}}

Using Eask as your Emacs package management tool.

{{< hint info >}}
The installation are cross-platform, using [npm](https://www.npmjs.com/).
For instruction about how to install Eask with other methods, see [install](https://emacs-eask.github.io/Getting-Started/Install-Eask/).

It is required to have [Git installed](https://git-scm.com/downloads)
to run this tutorial.
{{< /hint >}}

## Step 1: Install Eask

WIP

## Step 2: Navigate or Create an elisp project

WIP

## Step 3: Create `Eask`-file

WIP

---

```sh
$ npm install -g @emacs-eask/eask
```

Start by creating a file named `Eask` in the project root. Use **eask init**
command to create a Eask-file. You will be asked to enter few questions
to create the file.

```sh
$ eask init
```

To install all dependencies, run:

```sh
$ eask install-deps
```

{{< hint ok >}}
💡 *Use **-g** options for your Emacs configuration. Otherwise, it will create
a directory named **.eask** and install all dependencies into it.*
{{< /hint >}}

## 🔭 Finding Emacs

By default, Eask will use whatever the Emacs version exists in your environment
path. Use **emacs --version** to check your Emacs version.

There is currently no way to specify an Emacs version to execute.

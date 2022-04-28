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

## Step 1: Setup NodeJS runtime and `npm`

Please checkout their official site [here](https://docs.npmjs.com/downloading-and-installing-node-js-and-npm#using-a-node-installer-to-install-nodejs-and-npm)
and install `NodeJS` and `npm` corresponds to your current operating system

## Step 2: Install Eask

```sh
$ npm install -g @emacs-eask/eask
```

To verify your new install:

```sh
$ eask --version
```

## Step 3: Navigate to an existing project or create a new project

If you already have an existing elisp project, navigate to the project root
folder.

```sh
$ cd /path/to/project/dir/
```

To create one:

```sh
$ eask create project-name
```

It should create a folder named `project-name` in your current working directory.

{{< hint danger >}}
💡 This feature is still under development; hence it would **NOT** work!
{{< /hint >}}

## Step 4: Create `Eask`-file

If you choose to create project using the command `eask create`. The Eask-file
is generated automatically, hence you should skip this step.

Otherwise, do:

```sh
$ eask init
```

## Step 5: Start pacakge development

To install all dependencies, run:

```sh
$ eask install-deps
```

{{< hint ok >}}
💡 *Use **-g** options for your Emacs configuration. Otherwise, it will create
a folder named **.eask** and install all dependencies into it.*
{{< /hint >}}

## 🔭 Finding Emacs

By default, Eask will use whatever the Emacs version exists in your environment
path. Use **emacs --version** to check your Emacs version.

There is currently no way to specify an Emacs version to execute.

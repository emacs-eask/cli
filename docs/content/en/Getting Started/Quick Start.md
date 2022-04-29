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

## Step 4: Create `Eask`-file

If you choose to create project using the command `eask create`. The Eask-file
is generated automatically, hence you should skip this step.

Otherwise, do:

```sh
$ eask init
```

## Step 5: Start the pacakge development

To checkout your package information, run:

```sh
$ eask info
```

## Step 6: Manage package archives

You can manage package archives by using the `source` directive in your **Eask**-file.

```elisp
(source "gnu")    ; default
(source "melpa")  ; Add package archives
```

{{< hint info >}}
💡 See [DSL/source](https://emacs-eask.github.io/DSL/#-source-alias) for more information!
{{< /hint >}}

## Step 7: Add some dependencies

You can add dependenices by using `depends-on` directive in your **Eask**-file.

```elisp
...

(depends-on "f")
(depends-on "ht")
```

## Step 8: Install dependencies

WIP

## 🔭 Finding Emacs

By default, Eask will use whatever the Emacs version exists in your environment
path. Use **emacs --version** to check your Emacs version.

There is currently no way to specify an Emacs version to execute.

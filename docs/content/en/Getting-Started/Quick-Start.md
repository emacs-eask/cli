---
title: 🔰 Quick Start
weight: 100
---

{{< toc >}}

Using Eask as your Emacs package management tool.

{{< hint info >}}
The installation are cross-platform, using [npm](https://www.npmjs.com/).
For instructions about how to install Eask with other methods, see [install](https://emacs-eask.github.io/Getting-Started/Install-Eask/).

It is required to have [Git installed](https://git-scm.com/downloads)
to run this tutorial.
{{< /hint >}}

## Step 1: Setup NodeJS runtime and `npm`

Please check out their official site
[here](https://docs.npmjs.com/downloading-and-installing-node-js-and-npm#using-a-node-installer-to-install-nodejs-and-npm)
and install `NodeJS` and `npm` corresponds to your current operating system

{{< hint ok >}}
💡 If you don't prefer **NodeJS** and **npm**, you can install with [binary](https://emacs-eask.github.io/Getting-Started/Install-Eask/#binary-cross-platform)
from our [release](https://github.com/emacs-eask/cli/releases) page.
{{< /hint >}}

## Step 2: Install Eask

```sh
$ npm install -g @emacs-eask/cli
```

To verify your new installation:

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
$ eask create package project-name
```

It should create a folder named `project-name` in your current working directory.

## Step 4: Create `Eask`-file

Skip this step if you chose to create the project with **`eask create`**!

Otherwise, to create Eask-file in the existing project:

```sh
$ eask init
```

You will be asked some questions about the package you are going to create:

```
package name: (your-project)
version: (1.0.0)
description: Your project description!
entry point: (your-project.el)
emacs version: (26.1)
website: https://example.com/project-url/
keywords: tools example
About to write to /path/to/project/Eask:

(package "your-project"
         "1.0.0"
         "Your project description!")

(website-url "https://example.com/project-url/")
(keywords "tools" "example")

(package-file "your-project.el")

(script "test" "echo \"Error: no test specified\" && exit 1")

(source "gnu")

(depends-on "emacs" "26.1")


Is this OK? (yes) yes ⏎
```

You should be able to see an `Eask` file in your project folder. 🎉🎊

## Step 5: Start the package development

To check your package information, run:

```sh
$ eask info
```

You should be able to see the following information:

```
your-package (1.0.0) | deps: 0 | devDeps: 0
Your project description!
https://example.com/project-url/

keywords: tools, example

entry: your-package-file.el
kind: single

dist
.total-files: 0
.unpacked-size: 0
```

From the start, you would not have any `dependencies` and `devDependencies` (`0` by default)!

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

You can add dependencies by using `depends-on` directive in your **Eask**-file.

```elisp
...

(depends-on "f")
(depends-on "ht")
```

{{< hint danger >}}
💡 Make sure the dependencies you add are available in the package archives!

Or else you would get an error **`package-name-' is unavailable**!
{{< /hint >}}

## Step 8: Install dependencies

Now we can install the dependencies we have specified in the **Eask**-file:

```elisp
$ eask install-deps
```

You should see Eask executed correctly with the similar output below:

```
Loading package information... done
Installing 2 package dependencies...
  - Installing f (20220405.1534)... done
  - Installing ht (20210119.741)... done

(Total of 2 dependencies installed, 0 skipped)
```

## See Also

- [Commands and options](https://emacs-eask.github.io/Getting-Started/Commands-and-options/)
- [Domain Specific Language](https://emacs-eask.github.io/DSL/)
- [Basic Usage](https://emacs-eask.github.io/Getting-Started/Basic-Usage/)

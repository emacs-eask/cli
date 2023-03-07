---
title: 💾 Install Eask
weight: 200
---

This document guides you through the installation of Eask.

{{< toc >}}

## 💾 Quick Install

### Binary (Cross-platform)

Download the appropriate version for your platform from [Eask Releases](https://github.com/emacs-eask/cli/releases).
Once downloaded, the binary can be run from anywhere. You don’t need to install
it into a global location. This works well for shared hosts and other systems
where you don’t have a privileged account.

Ideally, you should install it somewhere in your `PATH` for easy use. `/usr/local/bin`
is the most probable location.

### Using Shell

Eask's [webinstall](https://github.com/emacs-eask/cli/tree/master/webinstall) 
provides convenience scripts to download and install the binary.

#### macOS and Linux

```
curl -fsSL https://raw.githubusercontent.com/emacs-eask/cli/master/webinstall/install.sh | sh
```

#### Windows

```
curl.exe -fsSL https://raw.githubusercontent.com/emacs-eask/cli/master/webinstall/install.bat | cmd /Q
```

### npm

If you have [npm](https://www.npmjs.com/) installed on your machine, you can
install Eask with the following one-liner:

```sh
$ npm install -g @emacs-eask/cli
```

### Homebrew (macOS or Linux)

WIP

### MacPorts (macOS)

WIP

### Chocolatey (Windows)

If you have [Chocolatey](https://chocolatey.org/) installed on your machine, you can
install Eask with the following one-liner:

```sh
$ chcoc install eask
```

### Scoop (Windows)

WIP

## 💾 Source

### 🚩 Prerequisite Tools

* [Git](https://git-scm.com/)
* [Node.js](https://nodejs.org/en/)
* [npm](https://www.npmjs.com/)

Alternatively, you can clone it directly from this repo

```sh
# clone the repo
$ git clone https://github.com/emacs-eask/cli eask

# change the working directory to eask
$ cd eask

# install the requirements
$ npm install
```

### 🏡 Setup

Make sure you set up the environment path variable according to your system,

On Linux/macOS,

```sh
export PATH="path/to/eask/bin:$PATH"
```

On Windows,

```batch
set PATH=%PATH%;c:/path/to/eask/bin
```

Once you have set it up correctly, try `eask --version` then you should see 
the current eask's version number! 🎉 🎊

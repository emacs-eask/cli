---
title: 💾 Install Eask
weight: 200
---

This document guides you through the installation of Eask.

Install Eask on macOS, Linux, Windows, BSD, and on any machine that can run the [Node.js][].

{{< toc >}}

## 💾 Prebuilt binaries

Download the appropriate version for your platform from [Eask Releases](https://github.com/emacs-eask/cli/releases).
Once downloaded, the binary can be run from anywhere. You don’t need to install
it in a global location. This works well for shared hosts and other systems
where you don’t have a privileged account.

Ideally, you should install it somewhere in your `PATH` for easy use. `/usr/local/bin`
is the most probable location.

## 💾 Using Shell

On macOS or Linux:

```sh
$ curl -fsSL https://raw.githubusercontent.com/emacs-eask/cli/master/webinstall/install.sh | sh
```

On Windows:

```sh
$ curl.exe -fsSL https://raw.githubusercontent.com/emacs-eask/cli/master/webinstall/install.bat | cmd /Q
```

## 💾 Package managers

### 📦 npm (Cross-platform)

If you have [npm](https://www.npmjs.com/) installed on your machine, you can
install Eask with the following one-liner:

```sh
$ npm install -g @emacs-eask/cli
```

### 📦 Homebrew (macOS or Linux)

[Homebrew](https://brew.sh/) is a free and open source package manager for
macOS and Linux. This will install the Eask CLI:

```sh
$ brew tap emacs-eask/cli https://github.com/emacs-eask/packaging
$ brew install eask-cli
```

### 📦 MacPorts (macOS)

[MacPorts](https://www.macports.org/) is a free and open source package manager for macOS.
This will install the Eask CLI:

```sh
$ sudo port install eask-cli
```

### 📦 Debian (Linux)

Derivatives of the [Debian][] distribution of Linux
include [elementary OS][], [KDE neon][], [Linux Lite][], [Linux Mint][],
[MX Linux][], [Pop!_OS][], [Ubuntu][], [Zorin OS][], and others.

```sh
$ sudo curl -SsL -o /etc/apt/trusted.gpg.d/easksource.gpg https://raw.githubusercontent.com/emacs-eask/packaging/master/debian/KEY.gpg
$ sudo curl -SsL -o /etc/apt/sources.list.d/easksource.list https://raw.githubusercontent.com/emacs-eask/packaging/master/debian/easksource.list
$ sudo apt update --allow-insecure-repositories
$ sudo apt install eask-cli --allow-unauthenticated
```

You can also download Debian packages from the 
[packaging](https://github.com/emacs-eask/packaging/tree/master/debian)
repo.

### 📦 Snap (Linux)

[Snap](https://snapcraft.io/) is a free and open source package manager for Linux.
Available for most distributions, snap packages are simple to install and are
automatically updated.

```sh
$ sudo snap install eask-cli
```

### 📦 Chocolatey (Windows)

If you have [Chocolatey](https://chocolatey.org/) installed on your machine, you can
install Eask with the following one-liner:

```sh
$ choco install eask-cli
```

### 📦 Scoop (Windows)

[Scoop](https://scoop.sh/) is a free and open source package manager for Windows.
This will install the Eask CLI:

```sh
$ scoop bucket add emacs-eask/cli https://github.com/emacs-eask/packaging
$ scoop install eask-cli
```

### 📦 Winget (Windows)

WIP

## 💾 Build from source

### 🚩 Prerequisite Tools

* [Git][]
* [Node.js][]
* [npm][]

Alternatively, you can clone it directly from this repo

```sh
# clone the repo
$ git clone https://github.com/emacs-eask/cli eask-cli

# change the working directory to eask-cli
$ cd eask-cli

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


[Git]: https://git-scm.com/
[Node.js]: https://nodejs.org/en/
[npm]: https://www.npmjs.com/

[Debian]: https://www.debian.org/
[elementary OS]: https://elementary.io/
[KDE neon]: https://neon.kde.org/
[Linux Lite]: https://www.linuxliteos.com/
[Linux Mint]: https://linuxmint.com/
[MX Linux]: https://mxlinux.org/
[Pop!_OS]: https://pop.system76.com/
[Ubuntu]: https://ubuntu.com/
[Zorin OS]: https://zorin.com/os/

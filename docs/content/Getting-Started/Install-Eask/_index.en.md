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

If you have [npm][] installed on your machine, you can
install Eask with the following one-liner:

```sh
$ npm install -g @emacs-eask/cli
```

### 📦 Homebrew (macOS or Linux)

[Homebrew][] is a free and open-source package manager for macOS and Linux.
To install the Eask CLI:

```sh
$ brew tap emacs-eask/cli https://github.com/emacs-eask/packaging
$ brew install eask-cli
```

### 📦 MacPorts (macOS)

[MacPorts][] is a free and open-source package manager for macOS.
To install the Eask CLI:

```sh
$ sudo port install eask-cli
```

### 📦 Debian (Linux)

Derivatives of the [Debian][] distribution of Linux include [elementary OS][],
[KDE neon][], [Linux Lite][], [Linux Mint][], [MX Linux][], [Pop!_OS][],
[Ubuntu][], [Zorin OS][], and others.

```sh
$ sudo curl -SsL -o /etc/apt/trusted.gpg.d/easksource.gpg https://raw.githubusercontent.com/emacs-eask/packaging/master/debian/KEY.gpg
$ sudo curl -SsL -o /etc/apt/sources.list.d/easksource.list https://raw.githubusercontent.com/emacs-eask/packaging/master/debian/easksource.list
$ sudo apt update --allow-insecure-repositories
$ sudo apt install eask-cli --allow-unauthenticated
```

You can also download Debian packages from the [packaging][] repo.

### 📦 Snap (Linux)

[Snap][] is a free and open-source package manager for Linux.
Available for most distributions, snap packages are simple to install and are
automatically updated.

```sh
$ sudo snap install eask-cli
```

### 📦 Arch (Linux)

There's a `PKGBUILD` that builds `eask` from sources and creates a package, so
inside the top directory of the repository you can simply run:

```sh
$ makepkg -i
```

### 📦 Chocolatey (Windows)

If you have [Chocolatey][] installed on your machine, you can
install Eask with the following one-liner:

```sh
$ choco install eask-cli
```

### 📦 Scoop (Windows)

[Scoop][] is a free and open-source package manager for Windows.
To install the Eask CLI:

```sh
$ scoop bucket add emacs-eask/cli https://github.com/emacs-eask/packaging
$ scoop install eask-cli
```

### 📦 Winget (Windows)

[Winget][] is Microsoft’s official free and open-source package manager for Windows.
To install the Eask CLI:

```
$ winget install eask.cli
```

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

### 🏡 Setup (through script)

You can now run `eask` using the script `bin/eask`; add `/path/to/eask-cli/bin/`
to your environment `PATH` to execute eask from any location!

On Linux/macOS,

```sh
export PATH="path/to/eask/bin:$PATH"
```

On Windows,

```batch
set PATH=%PATH%;c:/path/to/eask/bin
```

Once you have set it up correctly, try `eask --version` then you should see
the current `eask`'s version number! 🎉 🎊

### 🏡 Setup (through executable)

To run `eask` through executable, you will need [pkg][] installed on your machine.

```sh
# install it locally in the workspace scope
$ npm install --dev

# or

# install it globally
$ npm install -g pkg
```

Subsequently, run the following command to build the executable.
By default, it will generate an executable in the `dist` folder.

```sh
# build from sources. For available targets see `scripts` in `package.json`
$ npm run pkg-linux-x64

# move `lisp` to `dist` folder
mv lisp dist
```

You can now run `eask` using the executable `dist/eask`; add `/path/to/eask-cli/dist/`
to your environment `PATH` to execute eask from any location! 🎉 🎊


<!-- Links -->

[packaging]: https://github.com/emacs-eask/packaging/tree/master/debian

[Homebrew]: https://brew.sh/
[MacPorts]: https://www.macports.org/
[Snap]: https://snapcraft.io/
[Chocolatey]: https://chocolatey.org/
[Scoop]: https://scoop.sh/
[Winget]: https://learn.microsoft.com/en-us/windows/package-manager/

[Git]: https://git-scm.com/
[Node.js]: https://nodejs.org/en/
[npm]: https://www.npmjs.com/

[pkg]: https://github.com/vercel/pkg

[Debian]: https://www.debian.org/
[elementary OS]: https://elementary.io/
[KDE neon]: https://neon.kde.org/
[Linux Lite]: https://www.linuxliteos.com/
[Linux Mint]: https://linuxmint.com/
[MX Linux]: https://mxlinux.org/
[Pop!_OS]: https://pop.system76.com/
[Ubuntu]: https://ubuntu.com/
[Zorin OS]: https://zorin.com/os/

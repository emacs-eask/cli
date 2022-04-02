---
title: Introduction
---

Eask is heavily inspired by Cask, so they are somewhat related! This tool focuses
on consistency! [Cask]() and [makem.sh]() both rely on bash which Windows doesn't
run on by default. If you use WSL or other environment system file Cygwin/MSYS
; then this may not be the tool you are looking for! 👀

#### Why Node.JS?

Node has a better support on all kind of terminal applications (compare to just
the shell script)! Like colorful interface, entire npm community, etc; so you
can build cross-platform software with fewer hassles! Especially, after Microsoft
had bought the NPM inc, and would likely to support their own system well.

Cask does not seem to support Windows (no WSL) after version `0.8.6`. In the
early versions, they have used Python, but due to the Python supports on Windows
are just not as good as Node.JS.

#### Who should use this tool?

People who like to use Emacs on Windows (no WSL), and would like to keep their
Emacs configuration/packages consistent on every operating system!

## 💾 Installation

The easiest way to install is through tool [npm](https://www.npmjs.com/),

```sh
$ npm install -g @emacs-eask/eask
```

Alternatively, you can clone it directly from this repo

```sh
# clone the repo
$ git clone https://github.com/emacs-eask/eask

# change the working directory to eask
$ cd eask

# install the requirements
$ npm install
```

Make sure you set up the environment path variable according to your system,

On Linux/macOS,

```sh
PATH=$PATH:/path/to/eask/bin
```

On Windows,

```batch
set PATH=%PATH%;c:/path/to/eask/bin
```

Once you have set it up correctly, try `eask --version` then you should see 
the current eask's version number! 🎉 🎊

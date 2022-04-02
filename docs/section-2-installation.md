---
title: section-2-installation.md
permalink: installation
---

# 💾 Installation

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

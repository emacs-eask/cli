---
title: 💿 GitHub Actions
weight: 100
---

{{< toc >}}

Here is an example using the GitHub Actions service.

```yml
jobs:
  test:
    runs-on: ${{ matrix.os }}
    strategy:
      matrix:
        os: [ubuntu-latest, macos-latest, windows-latest]
        emacs-version: [27.2, snapshot]

    steps:
    - uses: actions/checkout@v2

    # Install Emacs
    - uses: jcs090218/setup-emacs@master
      with:
        version: ${{ matrix.emacs-version }}

    # You need node for eask
    - uses: actions/setup-node@v2
      with:
        node-version: '16'

    # Install eask
    - uses: emacs-eask/setup-eask@master
      with:
        version: 'snapshot'

    - name: Run tests
      run: |
        eask install
        eask compile
        eask lint
```

This example is testing your Emacs Lisp package in the below environment;

* Emacs: `27.2` and `snapshot`
* Node: `16.x`
* Eask: `snapshot` (latest)

with these following `actions`,

* [setup-emacs](https://github.com/purcell/setup-emacs) to install Emacs on Unix system (Linux/macOS)
* [setup-emacs-windows](https://github.com/jcs090218/setup-emacs-windows) to install Emacs on Windows
* [setup-node](https://github.com/actions/setup-node) to set up `Node` and `npm`
* [setup-eask](https://github.com/emacs-eask/setup-eask) to install desired Eask version

## 💾 Setup Eask locally

You can install Eask locally using scripts from `.github/scripts/setup-eask` (Unix)
or `.github/scripts/setup-eask.ps1` (Windows).

```yml
    - uses: actions/checkout@v2

    - name: Prepare Eask (Unix)
      if: matrix.os == 'ubuntu-latest' || matrix.os == 'macos-latest'
      run: |
        chmod -R 777 ./
        .github/scripts/setup-eask

    - name: Prepare Eask (Windows)
      if: matrix.os == 'windows-latest'
      run: .github/scripts/setup-eask.ps1
```

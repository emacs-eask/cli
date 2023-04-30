---
title: 💿 GitHub Actions
weight: 100
---

{{< toc >}}

[![Windows](https://img.shields.io/badge/-Windows-lightblue?logo=windows&style=flat&logoColor=blue)](#)
[![macOS](https://img.shields.io/badge/-macOS-lightgrey?logo=apple&style=flat&logoColor=white)](#)
[![Linux](https://img.shields.io/badge/-Linux-fcc624?logo=linux&style=flat&logoColor=black)](#)

Here is an example using the [GitHub](https://github.com/) Actions service.

```yml
jobs:
  test:
    runs-on: ${{ matrix.os }}
    strategy:
      matrix:
        os: [ubuntu-latest, macos-latest, windows-latest]
        emacs-version: [26.3, 27.2, 28.2, snapshot]

    steps:
    - uses: actions/checkout@v2

    # Install Emacs
    - uses: jcs090218/setup-emacs@master
      with:
        version: ${{ matrix.emacs-version }}

    # Install Eask
    - uses: emacs-eask/setup-eask@master
      with:
        version: 'snapshot'

    - name: Run tests
      run: |
        eask package
        eask install
        eask compile
```

This example is testing your Emacs Lisp package in the below environment;

| OS             | Emacs                              | Eask   |
|----------------|------------------------------------|--------|
| Linux (Ubuntu) | `26.x`, `27.x`, `28.x`, `snapshot` | latest |
| macOS          | `26.x`, `27.x`, `28.x`, `snapshot` | latest |
| Windows        | `26.x`, `27.x`, `28.x`, `snapshot` | latest |

with these following `actions`,

* [setup-emacs](https://github.com/jcs090218/setup-emacs) to install Emacs
* [setup-eask](https://github.com/emacs-eask/setup-eask) to install desired Eask version

{{< hint info >}}
💡 You can generate workflow file via `eask generate workflow github`, see 
[Commands and options](https://emacs-eask.github.io/Getting-Started/Commands-and-options/#-eask-generate-workflow-github)
for more information!
{{< /hint >}}

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

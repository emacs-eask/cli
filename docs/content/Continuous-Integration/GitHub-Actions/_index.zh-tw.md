---
title: 💿 GitHub Actions
weight: 100
---

{{< toc >}}

[![Windows](https://img.shields.io/badge/-Windows-lightblue?logo=windows&style=flat&logoColor=blue)](#)
[![macOS](https://img.shields.io/badge/-macOS-lightgrey?logo=apple&style=flat&logoColor=white)](#)
[![Linux](https://img.shields.io/badge/-Linux-fcc624?logo=linux&style=flat&logoColor=black)](#)

以下是使用 [GitHub](https://github.com/) Actions 服務的示例。

```yml
jobs:
  test:
    runs-on: ${{ matrix.os }}
    strategy:
      matrix:
        os: [ubuntu-latest, macos-latest, windows-latest]
        emacs-version: [26.3, 27.2, 28.2, 29.4, 30.2, snapshot]

    steps:
    - uses: actions/checkout@v3

    # 安裝 Emacs
    - uses: jcs090218/setup-emacs@master
      with:
        version: ${{ matrix.emacs-version }}

    # 安裝 Eask
    - uses: emacs-eask/setup-eask@master
      with:
        version: 'snapshot'

    - name: Run tests
      run: |
        eask package
        eask install
        eask compile
```

此示例在以下環境中測試您的 Emacs Lisp 包；

| OS             | Emacs                                              | Eask   |
|----------------|----------------------------------------------------|--------|
| Linux (Ubuntu) | `26.x`, `27.x`, `28.x`, `29.x`, `30.x`, `snapshot` | latest |
| macOS          | `26.x`, `27.x`, `28.x`, `29.x`, `30.x`, `snapshot` | latest |
| Windows        | `26.x`, `27.x`, `28.x`, `29.x`, `30.x`, `snapshot` | latest |

通過以下`操作`，

* [setup-emacs](https://github.com/jcs090218/setup-emacs) 安裝 Emacs
* [setup-eask](https://github.com/emacs-eask/setup-eask) 安裝所需的 Eask 版本

{{< hint info >}}
💡 您可以通過 `eask generate workflow github` 生成工作流文件，
參見[命令和選項](https://emacs-eask.github.io/Getting-Started/Commands-and-options/#-eask-generate-workflow-github)
了解更多信息！
{{< /hint >}}

## 💾 在本地設置 Eask

您可以使用 `.github/scripts/setup-eask` (Unix) 或 `.github/scripts/setup-eask.ps1` (Windows)
中的腳本在本地安裝 Eask。

```yml
    - uses: actions/checkout@v3

    - name: 準備 Eask (Unix)
      if: matrix.os == 'ubuntu-latest' || matrix.os == 'macos-latest'
      run: |
        chmod -R 777 ./
        .github/scripts/setup-eask

    - name: 準備 Eask (Windows)
      if: matrix.os == 'windows-latest'
      run: .github/scripts/setup-eask.ps1
```

---
title: 💠 CircleCI
weight: 400
---

{{< toc >}}

[![Windows](https://img.shields.io/badge/-Windows-lightblue?logo=windows&style=flat&logoColor=blue)](#)
[![macOS](https://img.shields.io/badge/-macOS-lightgrey?logo=apple&style=flat&logoColor=white)](#)
[![Linux](https://img.shields.io/badge/-Linux-fcc624?logo=linux&style=flat&logoColor=black)](#)

使用 [Circle CI](https://circleci.com/) 的示例。

```yml
version: 2.1

orbs:
  win: circleci/windows@2.2.0

# 對每個 Emacs 版本執行的默認操作
commands:
  setup-linux:
    steps:
      - checkout
      - run:
          name: Install unzip
          command: apt-get update && apt-get install unzip
      - run:
          name: Install Eask
          command: curl -fsSL https://raw.githubusercontent.com/emacs-eask/cli/master/webinstall/install.sh | sh

  setup-macos:
    steps:
      - checkout
      - run:
          name: Install Emacs latest
          command: |
            echo "HOMEBREW_NO_AUTO_UPDATE=1" >> $BASH_ENV
            brew install homebrew/cask/emacs
      - run:
          name: Install unzip
          command: apt-get update && apt-get install unzip
      - run:
          name: Install Eask
          command: curl -fsSL https://raw.githubusercontent.com/emacs-eask/cli/master/webinstall/install.sh | sh

  setup-windows:
    steps:
      - checkout
      - run:
          name: Install Eask
          command: url.exe -fsSL https://raw.githubusercontent.com/emacs-eask/cli/master/webinstall/install.bat | cmd /Q

  test:
    steps:
      - run:
          name: Run regression tests
          command: eldev -dtT -p test
  lint:
    steps:
      - run:
          name: Run Elisp-lint
          command: eldev lint
      - run:
          name: Byte-compile `.el' files
          command: eldev -dtT compile --warnings-as-errors

jobs:
  test-ubuntu-emacs-26:
    docker:
      - image: silex/emacs:26-ci
        entrypoint: bash
    steps:
      - setup-linux
      - test

  test-ubuntu-emacs-27:
    docker:
      - image: silex/emacs:27-ci
        entrypoint: bash
    steps:
      - setup-linux
      - test

  test-ubuntu-emacs-28:
    docker:
      - image: silex/emacs:28-ci
        entrypoint: bash
    steps:
      - setup-linux
      - test

  test-ubuntu-emacs-29:
    docker:
      - image: silex/emacs:29-ci
        entrypoint: bash
    steps:
      - setup-linux
      - test

  test-ubuntu-emacs-30:
    docker:
      - image: silex/emacs:30-ci
        entrypoint: bash
    steps:
      - setup-linux
      - test

  test-ubuntu-emacs-master:
    docker:
      - image: silex/emacs:master-ci
        entrypoint: bash
    steps:
      - setup-linux
      - test

  test-macos-emacs-latest:
    macos:
      xcode: "14.0.0"
    steps:
      - setup-macos
      - test

  test-windows-emacs-latest:
    executor: win/default
    steps:
      - run:
          name: Install Emacs latest
          command: |
            choco install emacs
      - setup-windows
      - test

workflows:
  version: 2
  ci-test-matrix:
    jobs:
      - test-ubuntu-emacs-26
      - test-ubuntu-emacs-27
      - test-ubuntu-emacs-28
      - test-ubuntu-emacs-29
      - test-ubuntu-emacs-30
      - test-ubuntu-emacs-master
      - test-macos-emacs-latest
      - test-windows-emacs-latest
```

此示例在以下環境中測試您的 Emacs Lisp 包；

| OS             | Emacs                                              | Eask   |
|----------------|----------------------------------------------------|--------|
| Linux (Ubuntu) | `26.x`, `27.x`, `28.x`, `29.x`, `30.x`, `snapshot` | latest |
| macOS          | `snapshot`                                         | latest |
| Windows        | `snapshot`                                         | latest |

{{< hint info >}}
💡 您可以通過 `eask generate workflow circle-ci` 生成工作流文件，
參見[命令和選項](https://emacs-eask.github.io/Getting-Started/Commands-and-options/#-eask-generate-workflow-circle-ci)
獲取更多信息！
{{< /hint >}}

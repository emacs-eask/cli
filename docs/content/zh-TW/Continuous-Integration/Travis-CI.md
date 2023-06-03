---
title: 📀 Travis CI
weight: 300
---

{{< toc >}}

[![Linux](https://img.shields.io/badge/-Linux-fcc624?logo=linux&style=flat&logoColor=black)](#)

使用 [Travis CI](https://www.travis-ci.com/) 的示例。

```yml
language: nix

os:
  - linux
  - osx

env:
  - EMACS_CI=emacs-26-3
  - EMACS_CI=emacs-27-2
  - EMACS_CI=emacs-28-2
  - EMACS_CI=emacs-snapshot

matrix:
  fast_finish: true
  allow_failures:
    - env: EMACS_CI=emacs-snapshot

install:
  - bash <(curl https://raw.githubusercontent.com/purcell/nix-emacs-ci/master/travis-install)
  - curl -fsSL https://raw.githubusercontent.com/emacs-eask/cli/master/webinstall/install.sh | sh
  - export PATH="$HOME/.local/bin:$PATH"

script:
  - eask package
  - eask install
  - eask compile
```

此示例在以下環境中測試您的 Emacs Lisp 包；

| OS             | Emacs                              | Eask   |
|----------------|------------------------------------|--------|
| Linux (Ubuntu) | `26.x`, `27.x`, `28.x`, `snapshot` | latest |
| macOS          | n/a                                | latest |
| Windows        | n/a                                | latest |


{{< hint info >}}
💡 您可以通過`eask generate workflow travis-ci`生成工作流文件，
參見[命令和選項](https://emacs-eask.github.io/Getting-Started/Commands-and-options/#-eask-generate- workflow-travis-ci)
了解更多信息！
{{< /hint >}}

---
title: 📀 Travis CI
weight: 300
---

{{< toc >}}

[![Linux](https://img.shields.io/badge/-Linux-fcc624?logo=linux&style=flat&logoColor=black)](#)

Example to use [Travis CI](https://www.travis-ci.com/).

```yml
language: nix

os:
  - linux
  - osx

env:
  - EMACS_CI=emacs-26-3
  - EMACS_CI=emacs-27-2
  - EMACS_CI=emacs-28-2
  - EMACS_CI=emacs-29-4
  - EMACS_CI=emacs-30-1
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

This example is testing your Emacs Lisp package in the below environment;

| OS             | Emacs                                              | Eask   |
|----------------|----------------------------------------------------|--------|
| Linux (Ubuntu) | `26.x`, `27.x`, `28.x`, `29.x`, `30.x`, `snapshot` | latest |
| macOS          | n/a                                                | latest |
| Windows        | n/a                                                | latest |


{{< hint info >}}
💡 You can generate workflow file via `eask generate workflow travis-ci`, see 
[Commands and options](https://emacs-eask.github.io/Getting-Started/Commands-and-options/#-eask-generate-workflow-travis-ci)
for more information!
{{< /hint >}}

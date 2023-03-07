---
title: 📀 Travis CI
weight: 300
---

{{< toc >}}

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

* Emacs: `26.x`, `27.x`, `28.x`, and `snapshot`
* Eask: `snapshot` (latest)

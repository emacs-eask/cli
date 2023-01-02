---
title: 📀 Travis CI
weight: 300
---

{{< toc >}}

Example to use [Travis CI](https://www.travis-ci.com/).

```yml
language: node_js
node_js:
  - 16

env:
  - EVM_EMACS=emacs-27.2-travis-linux-xenial
  - EVM_EMACS=emacs-git-snapshot-travis-linux-xenial

matrix:
  fast_finish: true
  allow_failures:
    - env: EVM_EMACS=emacs-git-snapshot-travis-linux-xenial

install:
  - npm install @emacs-eask/cli -g

script:
  - eask install
  - eask compile
  - eask lint package
```

This example is testing your Emacs Lisp package in the below environment;

* Emacs: `27.2` and `snapshot`
* Eask: `snapshot` (latest)

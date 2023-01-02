---
title: 🦊 GitLab Runner
weight: 200
---

{{< toc >}}

Example to use [GitLab](https://gitlab.com/) runner.

```yml
default:
  image: node:16

pages:
  stage: test
  before_script:
  - npm install @emacs-eask/cli -g
  - apt-get update
  - apt-get install emacs -y
  script:
  - eask install
  - eask compile
  - eask lint package
  only:
  - main
```

This example is testing your Emacs Lisp package in the below environment;

* Emacs: `snapshot`
* Eask: `snapshot` (latest)

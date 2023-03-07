---
title: 🦊 GitLab Runner
weight: 200
---

{{< toc >}}

Example to use [GitLab](https://gitlab.com/) runner.

```yml
default:
  before_script:
    - apt-get update
    - apt-get install unzip
    - curl -fsSL https://raw.githubusercontent.com/emacs-eask/cli/master/webinstall/install.sh | sh
    - export PATH="$HOME/.local/bin:$PATH"

test-26.3:
  image: silex/emacs:26.3-ci
  script:
    - eask clean all
    - eask package
    - eask install
    - eask compile

test-27.2:
  image: silex/emacs:27.2-ci
  script:
    - eask clean all
    - eask package
    - eask install
    - eask compile

test-28.2:
  image: silex/emacs:28.2-ci
  script:
    - eask clean all
    - eask package
    - eask install
    - eask compile
```

This example is testing your Emacs Lisp package in the below environment;

* Emacs: `26.x`, `27.x`, and `28.x`
* Eask: `snapshot` (latest)

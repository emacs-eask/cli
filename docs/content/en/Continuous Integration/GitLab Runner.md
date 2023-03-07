---
title: 🦊 GitLab Runner
weight: 200
---

{{< toc >}}

[![Linux](https://img.shields.io/badge/-Linux-fcc624?logo=linux&style=flat&logoColor=black)](#)

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

| OS             | Emacs                              | Eask   |
|----------------|------------------------------------|--------|
| Linux (Debian) | `26.x`, `27.x`, `28.x`, `snapshot` | latest |
| macOS          | n/a                                | latest |
| Windows        | n/a                                | latest |

{{< hint info >}}
💡 You can generate workflow file via `eask generate workflow gitlab`, see 
[Commands and options](https://emacs-eask.github.io/Getting-Started/Commands-and-options/#-eask-generate-workflow-gitlab)
for more information!
{{< /hint >}}

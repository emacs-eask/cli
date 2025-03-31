---
title: 🦊 GitLab Runner
weight: 200
---

{{< toc >}}

[![Linux](https://img.shields.io/badge/-Linux-fcc624?logo=linux&style=flat&logoColor=black)](#)

使用 [GitLab](https://gitlab.com/) 運行程序的示例。

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

test-29.4:
  image: silex/emacs:29.4-ci
  script:
    - eask clean all
    - eask package
    - eask install
    - eask compile

test-30.1:
  image: silex/emacs:30.1-ci
  script:
    - eask clean all
    - eask package
    - eask install
    - eask compile
```

此示例在以下環境中測試您的 Emacs Lisp 包；

| OS             | Emacs                                              | Eask   |
|----------------|----------------------------------------------------|--------|
| Linux (Debian) | `26.x`, `27.x`, `28.x`, `29.x`, `30.x`, `snapshot` | latest |
| macOS          | n/a                                                | latest |
| Windows        | n/a                                                | latest |

{{< hint info >}}
💡 您可以通過 `eask generate workflow gitlab` 生成工作流文件，
參見[命令和選項](https://emacs-eask.github.io/Getting-Started/Commands-and-options/#-eask-generate-workflow-gitlab)
了解更多信息！
{{< /hint >}}

---
title: 🚪 Introduction
weight: 0
---

Eask was built to use as a package development tool in your Elisp packages. But
now, Eask supports various types of Emacs Lisp tasks. It can be used in three
major ways:

1. Dev tool for Elisp packages
2. Dependency management for your configuration
3. Run elisp programs for all other purposes

So what are the major differences between Eask and other build tools like
[Cask][], [makem.sh][], and [Eldev][], other than the things above?

Good question! Eask is more than a build tool now, it can be used for various
purposes! But here are Eask aims to be:

- **Consistent** enough to sandbox across all systems
- **General** enough to have Emacsers frequently used commands (`byte-compile`, `checkdoc`, etc)
- **Robust** enough to provide useful results even in the presence of user errors
- **Dependency-free** so that the tool can be run on any platform

*P.S. See [Why Eask?](https://emacs-eask.github.io/#-why-eask) for more detailed
information.*

## ❓ Why Eask?

`Eask` has the same philosophy as Cask, see their site [Why Cask?](https://cask.readthedocs.io/en/latest/guide/introduction.html#introduction-why-cask)
to understand why you should use Eask (or Cask).

Many tools like [Cask][], [makem.sh][], or [Eldev][] don't **"really"** support
Windows. `Cask` has dropped support for Legacy Windows, `makem.sh` runs on bash,
`Eldev` does support Windows, but the author doesn't use it on Windows (not
having full tests, see their
[CI workflows](https://github.com/doublep/eldev/actions/workflows/test.yml)).
`Eask` aims to adapt all platforms, including `Linux`, `macOS`, and `Windows`.
It focuses on the cross-platform capability and the consistency between each
OS. If Eask works on your machine, then it will work on any platform.

Here are our suggestions; if you plan to work on an OS-specific package (never
going to other platforms), go for other tools. On the other hand, Eask aims to
provide the best consistency between each OS. Alternatively, if you want to
learn a tool that works everywhere, Eask is one of the best choices.

## ⚖️ Comparisons

The table was compiled by reading these projects’ documentation and source code,
but the author is not an expert on these tools. Corrections are welcome.

### 🔍 Project Wise

The table shows what technology has been chosen by their author and how the
project is being constructed. Furthermore, what technical decisions have they
made? Drop support? Project's layout? Etc.

|                | Eask              | Cask                       | Eldev          | makem.sh                   |
|----------------|-------------------|----------------------------|----------------|----------------------------|
| bin folder     | binary, bash, bat | bash, bat                  | bash, bat, ps1 | bash                       |
| Cross-Platform | ✅                | ❌, no [Windows][] support | ✅             | ❌, no [Windows][] support |
| Emacs version  | 26.1+             | 24.5+                      | 24.4+          | 26.1+                      |
| Size           | 9,000+ lines      | 3,000+ lines               | 8,000+ lines   | 1,200+ lines               |
| Executable     | ✅                | ❌                         | ❌             | ❌                         |
| Pure Elisp     | ❌, JavaScript    | ✅                         | ✅             | ✅                         |
| CLI Parser     | [yargs][]         | [commander][]              | built-in       | built-in                   |

{{< hint info >}}
💡 **makem.sh** has a good comparisons document as well, visit their [site](https://github.com/alphapapa/makem.sh#comparisons)
{{< /hint >}}

### 🔍 Feature Wise

This is the feature comparison between each tool. Every tool has its advantages;
choose the right tool that works for you!

If the features are not listed below, either it is forgotten or simply
considered too essential, so every tool has it; hence we don't add them to the
list.

|                           | Eask                                    | Cask                     | Eldev          | makem.sh |
|---------------------------|-----------------------------------------|--------------------------|----------------|----------|
| Elisp configuration       | ✅, [DSL][DSL-Eask] is optional         | ❌, [DSL][DSL-Cask] only | ✅, pure elisp | ❌       |
| Handle `archives` failure | ✅, see [archives][emacs-eask/archives] | ❌                       | ❌             | ❌       |
| `create` project, etc     | ✅                                      | ❌                       | ❌             | ❌       |
| `link` local dependencies | ✅                                      | ✅                       | ✅             | ❌       |
| `exec` program            | ✅                                      | ✅                       | ❌             | ❌       |
| `eval` expressions        | ✅                                      | ✅                       | ✅             | ❌       |
| `emacs` execution         | ✅                                      | ✅                       | ❌             | ❌       |
| Built-in `linters`        | ✅                                      | ❌                       | ✅             | ❌       |
| Built-in `tests`          | ✅                                      | ❌                       | ✅             | ❌       |
| Run script                | ✅                                      | ❌                       | ❌             | ❌       |
| Self-defined commands     | ✅                                      | ❌                       | ✅             | ❌       |
| Subcommand                | ✅                                      | ❌                       | ❌             | ❌       |

## 📰 News

- `0.9.x` - Improve UX in general
- `0.8.x` - Add `link` command
- `0.7.x` - Fix `default-directory` isn't honored by **-g** option
- `0.6.x` - You can now use `eask create` to create an Elisp project
- `0.5.x` - Handle error for failed archive
- `0.4.x` - Add color logger
- `0.3.x` - Add verbosity level and timestamps
- `0.2.x` - Done basic error handling with exit code at the end of executions
- `0.1.39` - Use `spawn` instead `exec`; now messages will be printed immediately
- `0.1.x` - Project bare-bones are pretty much complete!

## 📝 Todo list

### 🔍 Development

- [ ] [DEV] Publish package to [winget]()

### 🔍 Core commands

- [ ] [FEAT] Add `publish` command; to publish the package to the eask archive?

### 🔍 Eask-file commands

- N/A

## 📂 Underlying Projects

The design of Eask was greatly influenced by the following projects:

* [Cask][] - Project management tool for Emacs
* [makem.sh][] - Makefile-like script for building and testing Emacs Lisp packages
* [epm](https://github.com/xuchunyang/epm) - Emacs Package Manager
* [Eldev][] - Elisp Development Tool


<!-- Links -->

[emacs-eask/archives]: https://github.com/emacs-eask/archives
[Cask]: https://github.com/cask/cask
[makem.sh]: https://github.com/alphapapa/makem.sh
[Eldev]: https://github.com/doublep/eldev

[yargs]: https://github.com/yargs/yargs
[commander]: https://github.com/rejeep/commander.el

[DSL-Eask]: https://emacs-eask.github.io/DSL/
[DSL-Cask]: https://cask.readthedocs.io/en/latest/guide/dsl.html

[Windows]: https://www.microsoft.com/en-us/windows?r=1

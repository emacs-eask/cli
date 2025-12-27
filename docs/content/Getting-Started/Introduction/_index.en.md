---
title: 🚪 Introduction
weight: 0
---

Eask was originally designed as a package development tool for Elisp projects.
However, it has since expanded to support a wide range of Emacs Lisp tasks.
It can now be used in three major ways:

1. As a development tool for Elisp packages.
2. For managing dependencies in your Emacs configuration.
3. To run Elisp programs for a variety of purposes (essentially functioning as a runtime).

With these capabilities in mind, what sets Eask apart from other build tools
like [Cask][], [makem.sh][], and [Eldev][]?

Great question! Eask has evolved beyond just a build tool—it serves multiple purposes! Here’s what Eask aims to be:

- **Consistent**: Provides a reliable sandboxing environment across all systems.
- **Versatile**: Includes commonly used Emacs commands like `byte-compilation`, `checkdoc`, and more.
- **Robust**: Delivers useful results even when user errors occur.
- **Lightweight**: Runs on any platform without dependencies.

*📝 P.S. See [Why Eask?](https://emacs-eask.github.io/Getting-Started/Introduction/#-why-eask) for more detailed
information.*

## ❓ Why Eask?

`Eask` follows the same philosophy as [Cask][]. To understand why you should use
`Eask` (or [Cask][]), check out the [Why Cask?](https://cask.readthedocs.io/en/latest/guide/introduction.html#introduction-why-cask)
section on their website.

Many tools, such as [Cask][], [makem.sh][], and [Eldev][], don’t fully support Windows.
`Cask` has dropped support for Legacy Windows, `makem.sh` relies on Bash, and while
`Eldev` does support Windows, its author doesn’t actively use it on the platform,
meaning it lacks full testing (as seen in their CI workflows). In contrast, `Eask`
is designed to work across all major platforms, including Linux, macOS, and Windows
It prioritizes cross-platform compatibility and ensures consistency across different
operating systems. If `Eask` runs on your machine, it will work reliably on any platform.

Here’s our recommendation: if you’re developing an OS-specific package that will never
need cross-platform support, other tools may be a better fit. However, if you want a
tool that ensures seamless consistency across different operating systems, Eask is an excellent choice.

Another major advantage of `Eask` is its transparency—there are no hidden workflows
or obscure processes running in the background. Additionally, `Eask` strictly
avoids hacks or workaround fixes, ensuring that solutions are clean, maintainable,
and aligned with best practices.

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
| Support `docker`          | ✅                                      | ❌                       | ✅             | ❌       |
| Built-in `linters`        | ✅                                      | ❌                       | ✅             | ❌       |
| Built-in `tests`          | ✅                                      | ❌                       | ✅             | ❌       |
| Run script                | ✅                                      | ❌                       | ❌             | ❌       |
| Self-defined commands     | ✅                                      | ❌                       | ✅             | ❌       |
| Subcommand                | ✅                                      | ❌                       | ❌             | ❌       |

## 📰 News

- `0.12.x` - Correctly handle the exit code.
- `0.11.x` - Add commands `install-file` and `install-vc`.
- `0.10.x` - Add five new commands and improve the default user experience.
- `0.9.x` - Enhance overall user experience.
- `0.8.x` - Add `link` command.
- `0.7.x` - Fix `default-directory` isn't honored by **-g** option.
- `0.6.x` - You can now use `eask create` to create an Elisp project.
- `0.5.x` - Handle error for failed archive.
- `0.4.x` - Add color logger.
- `0.3.x` - Add verbosity level and timestamps.

## 📝 Todo list

### 🔍 Core commands

- [ ] [FEAT] Add `publish` command; to publish the package to the eask archive?

### 🔍 Eask-file commands

- N/A

## 📂 Underlying Projects

The design of Eask was greatly influenced by the following projects:

- [Cask][] - Project management tool for Emacs
- [makem.sh][] - Makefile-like script for building and testing Emacs Lisp packages
- [epm][] - Emacs Package Manager
- [Eldev][] - Elisp Development Tool


<!-- Links -->

[emacs-eask/archives]: https://github.com/emacs-eask/archives
[Cask]: https://github.com/cask/cask
[makem.sh]: https://github.com/alphapapa/makem.sh
[Eldev]: https://github.com/doublep/eldev
[epm]: https://github.com/xuchunyang/epm

[yargs]: https://github.com/yargs/yargs
[commander]: https://github.com/rejeep/commander.el

[DSL-Eask]: https://emacs-eask.github.io/DSL/
[DSL-Cask]: https://cask.readthedocs.io/en/latest/guide/dsl.html

[Windows]: https://www.microsoft.com/en-us/windows?r=1

---
title: ❓ How to Contribute
weight: 0
---

{{< toc >}}

## ⚜️ Code of Conduct

We have adopted the [Contributor Covenant](https://www.contributor-covenant.org/)
as its Code of Conduct, and we expect project participants to adhere to it.
Please read the full text so that you can understand what actions will and will
not be tolerated.

## 🪑 Open Development

All work on Eask happens directly on [GitHub](https://github.com/emacs-eask/eask).
Both core team members and external contributors send pull requests which go
through the same review process.

## 📌 Semantic Versioning

Eask follows [semantic versioning](https://semver.org/). We release patch versions
for critical bugfixes, minor versions for new features or non-essential changes,
and major versions for any breaking changes. When we make breaking changes, we
also introduce deprecation warnings in a minor version so that our users learn
about the upcoming changes and migrate their code in advance.

Every significant change is documented in the [changelog file](https://github.com/emacs-eask/eask/blob/master/CHANGELOG.md).

## 💡 Branch Organization

Submit all changes directly to the `master` branch. We don’t use separate branches
for development or for upcoming releases. We do our best to keep `master` in good shape,
with all tests passing.

Code that lands in `master` must be compatible with the latest stable release. It may
contain additional features, but no breaking changes. We should be able to release
a new minor version from the tip of `master` at any time.

## 📂 State of the project

The project's bare-bones are pretty much done, we are currently looking for
contributors to give us feedback and improve our TUI/UX for this tool!

We are also looking for advice to add more. Emacser often use commands
and options, so these features are prepared by default! Like command
`lint` (package-lint) or option `--debug` refers to `debug-on-error`
to `t`!

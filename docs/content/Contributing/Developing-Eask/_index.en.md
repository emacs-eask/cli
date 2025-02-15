---
title: 🔨 Developing Eask
weight: 20
---

{{< toc >}}

### 🚩 Prerequisites

To make changes to Eask, you should have:

1. [Node.js][] for the development environment.
2. [npm][] for the package manager.
3. [yargs][] for CLI parser.
4. [Emacs][], 26.1 or above!

### 📝 Building

To build the development environment, you would have to install Eask using
the [build from source](https://emacs-eask.github.io/Getting-Started/Install-Eask/#-build-from-source)
method. Make sure you have set up the environment PATH variable, so you can call
`eask` from the terminal.

After you have stepped through the installation, try:

```sh
eask locate
```

It should print out the location of the `eask` executable.
You should be able to identify the Eask executable's location,
even you have multiple Eask versions installed!

### 📈 Testing

Eask does not offer local testing, all our tests are accomplished using GitHub
Actions. Please fork our repository, and push your changes to your fork. GitHub
Actions should pick up the test for you!

Make sure you have GitHub Actions enabled in your repository (forked). Got to
**Settings** -> **Actions** -> **General** -> **Actions Permissions**; make sure
you have checked the correct options.


<!-- Links -->

[Node.js]: https://nodejs.org/en/
[npm]: https://www.npmjs.com/
[yargs]: https://github.com/yargs/yargs
[Emacs]: https://www.gnu.org/software/emacs/
